(*                                                                          *)
(* (c) 2004, Anastasia Gornostaeva. <ermine@ermine.pp.ru                    *)
(*                                                                          *)

open Xml
open Xmpp
open Types
open Sqlite
open Sqlite_util
open Muc
open Netstring_str

let _ = Random.self_init ()

let db =
   let dbf = Sqlite.db_open "./markov.db" in
      if not (result_bool dbf
	"SELECT name FROM SQLITE_MASTER WHERE type='table' AND name='words'")
      then
	 (try exec dbf 
     "CREATE TABLE words (word1 varchar(256), word2 varchar(256), counter int);"
	  with Sqlite_error s -> 
	     raise (Failure "error while creating table"));
      dbf

let bad_words = ["бля"; "бляд"; "блять"; "пизд"; "хуй"; "нахуй"; "нехуй";
		"хуево"; "нехуево"; "мудак"; "сука"; "нохуй"; "похуй"; 
		"нахуя"; "нехуя"; "нохуя"]

let add words =
   let rec cycle w1 lst =
      match lst with
	 | [] ->
	      let cond = ("word1=" ^ escape w1 ^ " AND word2=''") in 
		 if result_bool db
		    ("SELECT counter FROM words WHERE " ^ cond) then begin
		       Unix.sleep 1;
		       exec db 
			  ("UPDATE words SET counter=counter+1 WHERE " ^ cond)
		    end
		 else
		    exec db ("INSERT INTO words VALUES(" ^ 
			     escape w1 ^ ",'',1)");
	 | w2 :: tail ->
	      let cond = ("word1=" ^ escape w1 ^ " AND word2=" ^ 
			  escape w2) in
		 if result_bool db
		    ("SELECT counter FROM words WHERE " ^ cond) then
		       exec db 
			  ("UPDATE words SET counter=counter+1 WHERE " ^ cond)
		 else
		    exec db ("INSERT INTO words VALUES(" ^ 
			     escape w1 ^ "," ^ escape w2 ^ ",1)");
		 cycle w2 tail
   in
      cycle "" words

let seek (w1:string) =
   let sum = result_integer db
		("SELECT sum(counter) FROM words WHERE word1=" ^
		 escape w1) in
      if sum = 0 then
	 w1, ""
      else
	 let vm = compile_simple db 
		     ("SELECT word1, word2, counter FROM words WHERE word1=" ^
		      escape w1) in
	 let rec cycle lsum =
	    try 
	       let data = step_simple vm in
		  if lsum - int_of_string data.(2) <= 0 then begin
		     finalize vm;
		     data.(0), data.(1)
		  end
		  else
		     cycle (lsum - int_of_string data.(2))
	    with Sqlite_done -> w1, ""
	 in
	    cycle  (Random.int sum)

let generate word =
   let rec cycle w =
      let w1, w2 = seek w in
	 if w2 = "" then ""
	 else
	    w2 ^ " " ^ cycle w2
   in cycle word

let nick_rex = regexp "\\([^:]+\\)\\(:[ \r\n\t]*\\)"

(* let r = regexp "[ \n\r]*\\([^\\.\\?\\!!]+[\\.\\!\\?]\п\)" *)
let me = regexp "/me"
(* "\\([.?!][]\"')}]*\\($\\|\t\\)[ \t\r\n]*" *)
(* "[.?!][]\"')}]*\\($\\|\t\\| \\)[ \t\n]*" *)

let markov_chain xml out bot mynick lang =
   let body = try Xml.get_cdata xml ~path:["body"] with _ -> "" in
   let from = get_attr_s xml "from" in
   let nick, text =
      match string_match ~groups:2 nick_rex body 0 with
	 | None ->
	      "", body
	 | Some r ->
	      let nick = matched_group r 1 body in
		 if nick = mynick then
		    nick, string_after body (group_end r 2)
		 else
		    let room = get_bare_jid from in
		    let nmap = PMap.find room !participants in
		       if Nicks.mem nick nmap then
			  nick, 
			  string_after body (group_end r 2)
		       else
			  "", body
   in
      if text <> "" then
	 let words = split (regexp "[ \t\n]+") text in
Printf.printf "Orig: %s\n" text;
List.iter (function w -> Printf.printf "%s " w) words;
print_newline();

	    if List.exists (function word ->
			       List.mem word bad_words
			   ) words then
	       let author = get_resource from in
	       let room = get_bare_jid from in
	       let id = new_id () in
		  out (Muc.kick id room author "фильтруем базар");
		  let proc x = 
		     match get_attr_s x "type" with
			| "error" ->
			     let err_text =  
				try 
				   get_cdata ~path:["error"; "text"] x 
				with _ -> "Хм... наверное модератор?"
			     in
				out (make_msg xml (author ^ ": " ^ err_text))
			| _ -> ()

		  in
		     Event.sync 
			(Event.send bot (RegisterHandle (Id (id, proc))))
	    else begin
	       add words;

	       if safe_get_attr_s xml "type" = "groupchat" then
		  if nick = mynick then
		     let room = get_bare_jid from in
		     let asker = get_resource from in
		     let g = generate "" in
		     let reply =
			match string_match me g 0 with
			   | Some r -> g
			   | None ->
				asker ^ ": " ^ g
		     in
			out (outmsg room reply)
		  else
		     ()
	       else
		  out (make_msg xml (generate ""))
	    end

let markov_count xml out bot mynick lang =
   let result = result_integer db "SELECT COUNT(*) FROM words" in
      out (make_msg xml (string_of_int result))

let markov_top xml out bot mynick lang =
   let vm = compile_simple db 
	       "SELECT word1, word2, counter FROM words WHERE word1!='' AND word2!='' ORDER BY counter DESC LIMIT 10"
   in
   let rec cycle () =
      try 
	 let data = step_simple vm in
	    (Printf.sprintf "\n%s | %s | %s"
		data.(0) data.(1) data.(2))
	    ^ cycle ()
      with Sqlite_done -> ""
   in
   let top = cycle () in
      out (make_msg xml top)
(*
let markov_turn_off xml out bot mynick lang =
   let from = get_attr_s xml "from" in
   let nick = get_resourse from in
      if nick = "ermine" then begin
	 Muc.unregister_catch "markov_chain";
	 out (make_msg xml "Всё, буду молчать!")
      end
      else
	 out (make_msg xml ":-P")

let markov_turn_on xml out bot mynick lang =
   let from = get_attr_s xml "from" in
   let nick = get_resourse from in
      if nick = "ermine" then begin
	 Muc.register_catch "markov_chain" markov_chain;
	 out (make_msg xml "Ага.")
      end
*)
(*
let sql_rex = "!!!sql +\\(.+\\)$"

let markov_sql xml oyt bot mynick lang =
   let body = try Xml.get_cdata xml ~path:["body"] with _ -> "" in
      if string_match sql_rex body 0 then
	 let sql = matched_group 1 body in
	 let vm = compile_simple db sql in
	 let rec cycle () in
	 let data = step_simple vm in
*)	    
	 
let _ =
   Muc.register_catch markov_chain;
   Muc.register_cmd "!!!count" markov_count;
   Muc.register_cmd "!!!top" markov_top;
(*
   Muc.register_cmd "замолчи" makrov_turn_off;
   Muc.register_cmd "говори" makrov_turn_on
*)
