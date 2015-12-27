(*
 * (c) 2009-2011 Anastasia Gornostaeva
 *)

let _ =
  let fin = open_in Sys.argv.(1) in
  let buf = Buffer.create 1024 in
  let () =
    try while true do Buffer.add_string buf (input_line fin) done
    with End_of_file -> close_in fin in
    
  let getchar () = int_of_char (input_char stdin) in
  let putchar ch = print_char (Char.chr (ch land 0xFF)) in
  let code = Buffer.contents buf in
    Brainfuck.execute code putchar getchar
