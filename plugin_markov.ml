(*                                                                          *)
(* (c) 2004, Anastasia Gornostaeva. <ermine@ermine.pp.ru>                    *)
(*                                                                          *)

open Xml
open Xmpp
open Common
open Sqlite
open Sqlite_util
open Hooks

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
		    ("SELECT counter FROM words WHERE " ^ cond) then
		       exec db 
			  ("UPDATE words SET counter=counter+1 WHERE " ^ cond)
		 else
		    exec db ("INSERT INTO words VALUES(" ^ 
			     escape w1 ^ ",'',1)");
	 | w2 :: tail ->
	      if w1 = w2 then
		 cycle w2 tail
	      else begin
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
	      end
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
	    with Sqlite_done -> 
	       w1, ""
	 in
	    cycle (Random.int sum + 1)

let generate word =
   let rec cycle w i =
      let w1, w2 = seek w in
	 if w2 = "" then ""
	 else
	    w2 ^ " " ^ cycle w2 (i+1)
   in cycle word 0

let bad_words_here words =
   List.exists (function word -> List.mem word bad_words ) words

let split_words body =
   Pcre.split ~pat:"[ \t\n]+" body

let process_groupchat body xml out =
   let from = get_attr_s xml "from" in
   let room = get_bare_jid from in
   let author = get_resource from in
   let room_env = GroupchatMap.find room !groupchats in
      match safe_get_attr_s xml "type" with
	 | "groupchat" ->
	      if author <> room_env.mynick then
		 let nick, text = split_nick_body room_env body in
		 let words = split_words text in
		    if words = [] then begin
		       if nick = room_env.mynick then
			  out (make_msg xml "?")
		    end
		    else begin
		       if bad_words_here words then	       
			  let id = Hooks.new_id () in
			     out (Muc.kick id room author 
				     (Lang.get_msg ~xml
					 "plugin_markov_kick_reason" []));
			     let proc x o = 
				match get_attr_s x "type" with
				   | "error" ->
					let err_text = try
					   get_error_semantic x
					with Not_found -> 
					   Lang.get_msg ~xml 
					      "plugin_markov_kick_error" []
					in
					   out (make_msg xml (err_text))
				   | _ -> ()
			     in
				Hooks.register_handle (Hooks.Id (id, proc))
		       else
			  add words;
		       if nick = room_env.mynick then
			  let chain = generate "" in
			     out (make_msg xml chain)
		       else
			  ()
		    end
	      else
		 ()
	 | _ ->
	      let words = split_words body in
		 if bad_words_here words then	       
		    let id = Hooks.new_id () in
		       out (Muc.kick id room author 
			       (Lang.get_msg ~xml 
				   "plugin_markov_kick_reason" []));
		       let proc x o = 
			  match get_attr_s x "type" with
			     | "error" ->
				  let err_text = try
				     get_error_semantic x
				  with Not_found -> 
				     Lang.get_msg ~xml
					"plugin_markov_kick_error" []
				  in
				     out (make_msg xml err_text)
			     | _ -> ()
		       in
			  Hooks.register_handle (Hooks.Id (id, proc))
		 else begin
		    add words;
		    let chain = generate "" in
		       out (make_msg xml chain)
		 end

let markov_chain xml out =
   if not (mem_xml xml ["message"] "subject" []) then
      let body = try skip_ws (get_cdata xml ~path:["body"]) with _ -> "" in
	 if body <> "" then
	    (* reset idle *)
	    let from = get_attr_s xml "from" in
	    if GroupchatMap.mem (get_bare_jid from) !groupchats then
	       process_groupchat body xml out
	    else
	       let words = split_words body in
		  if bad_words_here words then
		     out (make_msg xml 
			     (Lang.get_msg ~xml 
				 "plugin_markov_phrase_is_ignored" []))
		  else begin
		     add words;
		     let chain = generate "" in
			out (make_msg xml chain)
		  end

let markov_count text xml out =
   let result = result_integer db "SELECT COUNT(*) FROM words" in
      out (make_msg xml (string_of_int result))

let markov_top text xml out =
   let vm = compile_simple db 
      "SELECT word1, word2, counter FROM words WHERE word1!='' AND word2!='' \
       ORDER BY counter DESC LIMIT 10"
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
let markov_turn_off xml out =
   let from = get_attr_s xml "from" in
   let nick = get_resourse from in
      if nick = "ermine" then begin
	 Muc.unregister_catch "markov_chain";
	 out (make_msg xml "Всё, буду молчать!")
      end
      else
	 out (make_msg xml ":-P")

let markov_turn_on xml out bot =
   let from = get_attr_s xml "from" in
   let nick = get_resourse from in
      if nick = "ermine" then begin
	 Muc.register_catch "markov_chain" markov_chain;
	 out (make_msg xml "Ага.")
      end
*)
(*
let sql_rex = "!!!sql +\\(.+\\)$"

let markov_sql xml oyt =
  let body = try Xml.get_cdata xml ~path:["body"] with _ -> "" in
  if string_match sql_rex body 0 then
  let sql = matched_group 1 body in
  let vm = compile_simple db sql in
  let rec cycle () in
  let data = step_simple vm in
*)	    

let _ =
   register_handle (Catch markov_chain);
   register_handle (Command ("!!!count", markov_count));
   register_handle (Command ("!!!top", markov_top))
(*
   Muc.register_cmd "замолчи" makrov_turn_off;
   Muc.register_cmd "говори" makrov_turn_on
*)
