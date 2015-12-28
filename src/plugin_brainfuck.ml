(*
 * (c) 2010 Anastasia Gornostaeva
 *)

open Hooks
open Plugin_command

let encode str =
  let buf = Buffer.create (String.length str) in
    String.iter (function
                   | ('\009'| '\x0A') as ch -> Buffer.add_char buf ch
                   | '\x0D' -> ()
                   | '\032'..'\126' as ch -> Buffer.add_char buf ch
                   | ch -> Buffer.add_string buf
                       (Printf.sprintf "\\x%02X" (Char.code ch))
                ) str;
    Buffer.contents buf
  
let bf xmpp env kind jid_from text =
  if text = "" then
    env.env_message xmpp kind jid_from "brain fuck!"
  else
    let code = Xml.decode text in
    let getchar () = int_of_char '.' in
    let buf = Buffer.create 1023 in
    let putchar ch = Buffer.add_char buf (Char.chr (ch land 0xFF)) in
    let f () =
      try
        Brainfuck.execute code putchar getchar;
        let result = Buffer.contents buf in
          if result <> "" then
            env.env_message xmpp kind jid_from (encode (encode result))
      with
        | Brainfuck.Error msg ->
            env.env_message xmpp kind jid_from msg
        | exn ->
            env.env_message xmpp kind jid_from (Printexc.to_string exn)
    in
      ignore (Thread.create f ())

let plugin opts =
  add_for_token
    (fun _opts xmpp ->
       add_commands xmpp [("bf", bf)] opts
    )

let _ =
  Plugin.add_plugin "brainfuck" plugin
