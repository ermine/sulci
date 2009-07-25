(*
 * (c) 2009 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Xml
open XMPP
open Types
open Common
open Hooks

let encode str =
  let buf = Buffer.create (String.length str) in
    String.iter (function
                   | '\032'..'\126' as ch -> Buffer.add_char buf ch
                   | ch -> Buffer.add_string buf
                       (Printf.sprintf "\\x%02X" (Char.code ch))
                ) str;
    Buffer.contents buf
  
let bf text _from xml _env out =
  if text = "" then
    make_msg out xml "brain fuck!"
  else
    let code = decode text in
    let getchar () = int_of_char '.' in
    let buf = Buffer.create 1023 in
    let putchar ch = Buffer.add_char buf (Char.chr (ch land 0xFF)) in
    let f () =
      try
        Brainfuck.execute code putchar getchar;
        let result = Buffer.contents buf in
          if result <> "" then
            make_msg out xml (encode (encode result))
      with
        | Brainfuck.Error msg ->
            make_msg out xml msg
        | exn ->
            make_msg out xml (Printexc.to_string exn)
    in
      ignore (Thread.create f ())

let _ =
  register_command "bf" bf
