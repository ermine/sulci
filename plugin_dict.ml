(*                                                                          *)
(* (c) 2004, Anastasia Gornostaeva. <ermine@ermine.pp.ru                    *)
(*                                                                          *)

open Types

exception DictError of string

let connect server port =
   let inet_addr =
      try Unix.inet_addr_of_string server with Failure("inet_addr_of_string") ->
         (Unix.gethostbyname server).Unix.h_addr_list.(0) in
   let sock_addr = Unix.ADDR_INET (inet_addr, port) in
      Unix.open_connection sock_addr

exception NoStatus

let status = Str.regexp "\\([0-9][0-9][0-9]\\) \\(.*\\)"

let get_status in_dict =
   let line = input_line in_dict in
      if Str.string_match status line 0 then
	 Str.matched_group 1 line, Str.matched_group 2 line
      else
	 raise (DictError "Unable to connect")

let read_text in_dict =
   let rec cycle acc =
      let line = input_line in_dict in
	 if line = ".\r" then
	    acc
	 else
	    cycle (acc ^ line ^ "\n")
   in
      cycle ""

let process_cmd_dict cmd =
   if List.mem cmd ["list"] then
      let in_dict, out_dict = connect "localhost" 2628 in
      let reply = 
	 (match get_status in_dict with
	     | "220", _ ->
		  (match cmd with
		      | "list" ->
			   output_string out_dict "SHOW DB\r\n";
		  );
		  flush out_dict;
		  (match get_status in_dict with
		      | "110", rsp ->
			   let piece = read_text in_dict in
			   let _ = get_status in_dict in
			      rsp ^ "\n" ^ piece
		      | "554", rsp ->
			   rsp
		      | code, rsp ->
			   raise (DictError ("Unknown status code: " ^ 
						  code ^ " " ^ rsp))
		  );
	     | _ -> 
		  raise (DictError "Cannot work!")
	 ) in
	 close_in in_dict;
	 reply
   else
      "Unknown command"

let process_dict db word =
   let in_dict, out_dict = connect "localhost" 2628 in
   let reply = match get_status in_dict with
      | "220", _ ->
	   output_string out_dict 
	       (Printf.sprintf "DEFINE %s %s\r\n" db word);
	   flush out_dict;
	   (match get_status in_dict with
	       | "550", err -> "Нет такой базы данных, юзай dict -list"
	       | "552", err -> "Не найдено ничего подходящего."
	       | "150", rsp -> 
		    let rec cycle acc =
		       match get_status in_dict with
			  | "151", text ->
			       let piece = read_text in_dict in
				  cycle (acc ^ "\n" ^ text ^ "\n" ^ piece)
			  | "250", _ ->
			       acc
			  | code, txt -> 
			       raise (DictError ("unknown dict status " ^ 
						 code ^ " " ^ txt))
		    in
		       cycle rsp;
	       | "151", rsp ->
		    let piece = read_text in_dict in
		    let _ = get_status in_dict in
		       rsp ^ "\n" ^ piece
	       | code, rsp ->
		    raise (DictError ("Unknown status code " ^
				      code ^ " " ^ rsp))
	   )
      | _ -> "Cannot work!"
   in
      close_in in_dict;
      reply

let rex_cmd = Str.regexp "dict +-\\([a-z]+\\)[\n\r ]*$"
let rex1 = Str.regexp "dict +\\([^ \n\t\r]+\\)[\n\r ]*$"
let rex2 = Str.regexp "dict +\\(!\\|\\*\\|[a-z]+\\) +\\([a-z]+\\)"

let dict xml out bot mynick lang =
   let body = try Xml.get_cdata xml ~path:["body"] with _ -> "" in
      if Str.string_match rex_cmd body 0 then
	 let cmd = Str.matched_group 1 body in
	 let proc x =
	    let response = try
	       process_cmd_dict cmd
	    with DictError error -> error
	    in 
	       out (make_msg xml (Xml.crypt response))
	 in
	    ignore (Thread.create proc xml)
      else if Str.string_match rex2 body 0 then
	 let db = Str.matched_group 1 body in
	 let word = Str.matched_group 2 body in
	 let proc x =
	    let response = try
	       process_dict db word
	    with (DictError error) -> error
	    in
	       out (make_msg xml (Xml.crypt response))
	 in
	    ignore (Thread.create proc xml)
      else if Str.string_match rex1 body 0 then
	 (* let db = Str.matched_group 1 body in *)
	 let word = Str.matched_group 1 body in
	 let proc x =
	    let response = try	    
	       process_dict "*" word
	    with (DictError error) -> error
	    in
	       out (make_msg xml (Xml.crypt response))
	 in
	    ignore (Thread.create proc xml)
(*
let start () =
   let in_dictd, out_dictd = connect "localhost" 2628 in
   let line = input_line in_dictd in
      if get_status line = ("220", _) then
	 ignore (Thread.create loop ())
*)

let _ =
   Muc.register_cmd "dict" dict;
   Muc.register_help "dict"
"dict слово
   Поиск определений слова в словарях
dict -list
   Список словарей
dict имя_базы слово
dict имя_базы \"словосочетание\"
вшсе ! слово
   Поиск слова в словарях и вывод только первого найденного определения
dict * слово
   Поиск слова в словарях и вывод всех найденных определений"
