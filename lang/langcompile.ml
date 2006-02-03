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

let prepare msg =
   let tab = String.create 1 in
      tab.[0] <- '\t';
      let rec cycle part =
	 try
	    let mark = String.index part '\\' in
	       if part.[mark+1] = 't' then
		  (String.sub part 0 mark) ^ tab ^ 
		     cycle (string_after part (mark+2))
	       else
		  String.sub part 0 (mark+2) ^
		     (cycle (string_after part (mark+2)))
	 with Not_found ->
	    part
      in
	 cycle msg
	 
let import () =
   let lang = Sys.argv.(2)
   and file = Sys.argv.(3) in
   let fin = open_in file in
   let mout = open_out_bin (lang ^ ext) in
   let hst = Hashtbl.create 10 in
      
   let rec cycle () =
      let line = input_line fin in
	 if String.length line >= 2 &&
	    String.sub line 0 2 = "//" || skip_ws line = "" then
	    cycle ()
	 else
	    let s = String.index line ' ' in
	    let key = String.sub line 0 s in
	    let msg = skip_ws (string_after line s) in
	       Hashtbl.add hst key (prepare msg);
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
langcompile diff <file1.htbl> <file2.htbl>

Examples:
           # import ru.msg to ru.htbl
	   langcompile import ru ru.msg
           # export from fr.htbl to text file
	   langcompile export fr fr.text"


let diff () =
   let f1 = input_value (open_in_bin Sys.argv.(2)) in
   let f2 = input_value (open_in_bin Sys.argv.(3)) in
      Hashtbl.iter (fun (i:string) _ ->
		       try let _ = Hashtbl.find f2 i in () with Not_found ->
			  print_endline i
		   ) f1

let _ =
   if Array.length Sys.argv = 1 then
      usage ()
   else
      match Sys.argv.(1) with
	 | "import" -> import ()
	 | "export" -> export ()
	 | "diff"   -> diff ()
	 | _        -> usage ()

