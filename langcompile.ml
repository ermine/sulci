(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

let ext = ".htbl"

let string_after s n =
  String.sub s n (String.length s - n)

let skip_ws str =
   if str = "" then str
   else
      let rec cycle i =
	 if i = String.length str then ""
	 else
	    if List.mem str.[i] [' '; '\n'; '\r'; '\t'] then cycle (i+1)
	    else if i > 0 then string_after str i
	    else str
      in
	 cycle 0

let import () =
   let lang = Sys.argv.(2)
   and file = Sys.argv.(3) in
   let fin = open_in file in
   let mout = open_out_bin (lang ^ ext) in
   let hst = Hashtbl.create 10 in
      
   let rec cycle () =
      let line = input_line fin in
      let s = String.index line ' ' in
      let key = String.sub line 0 s in
      let msg = skip_ws (string_after line s) in
	 Hashtbl.add hst key msg;
	 cycle ()
   in
   let () =
      try 
	 cycle ()
      with End_of_file -> 
	 Marshal.to_channel mout hst [];
	 flush mout 
   in
      close_in fin;
      close_out mout

let export () =
   let lang = Sys.argv.(2) 
   and file = Sys.argv.(3) in
   let fout = open_out file in
   let min = open_in_bin (lang ^ ext) in
   let hst = Marshal.from_channel min in
      Hashtbl.iter (fun k v ->
		       output_string fout (Printf.sprintf "%s   %s\n" k v)) hst;
      flush fout;
      close_out fout;
      close_in min

let usage () =
   print_endline "Usage:
langcompile import <lang> <file>
langcompile export <lang> <file>

Examples:
           # import ru.msg to ru.htbl
	   langcompile import ru ru.msg
           # export from fr.htbl to text file
	   langcompile export fr fr.text"


let _ =
   if Array.length Sys.argv = 1 then
      usage ()
   else
      match Sys.argv.(1) with
	 | "import" -> import ()
	 | "export" -> export ()
	 | _ -> usage ()

