(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xml
open Xmpp
open Common
open Sqlite
open Sqlite_util
open Hooks

type mevent = 
   | MMessage of Xml.element * (Xml.element -> unit) 
   | MStop 
   | MCount of Xml.element * (Xml.element -> unit)
   | MTop of Xml.element * (Xml.element -> unit)

module MarkovMap = Map.Make(Id)
let markovrooms = ref MarkovMap.empty

let _ = Random.self_init ()

let open_markovdb room =
   let path = 
      try trim (Xml.get_attr_s Config.config 
		   ~path:["plugins"; "markov"] "dir")
      with Not_found -> "./markov_db/" in
      if not (Sys.file_exists path) then Unix.mkdir path 0o755;
      let db = Sqlite.db_open ("./markov_db/" ^ room) in
	 if not (result_bool db
	   "SELECT name FROM SQLITE_MASTER WHERE type='table' AND name='words'")
	 then begin try
	    exec db
     "CREATE TABLE words (word1 varchar(256), word2 varchar(256), counter int)";
	    exec db "create index word1word2 on words(word1, word2)"
	 with Sqlite_error s -> 
	    raise (Failure "error while creating table")
	 end;
	 db

let bad_words = ["бля"; "бляд"; "блять"; "пизд"; "хуй"; "нахуй"; "нехуй";
		"хуево"; "нехуево"; "мудак"; "сука"; "нохуй"; "похуй"; 
		"нахуя"; "нехуя"; "нохуя"]

let add db words =
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

let seek db (w1:string) =
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

let generate db word =
   let rec cycle w i =
      let w1, w2 = seek db w in
	 if w2 = "" then ""
	 else
	    w2 ^ " " ^ cycle w2 (i+1)
   in cycle word 0

let bad_words_here words =
   List.exists (function word -> List.mem word bad_words ) words

let split_words body =
   Pcre.split ~pat:"[ \t\n]+" body

let process_groupchat body db xml out =
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
			  add db words;
		       if nick = room_env.mynick then
			  let chain = generate db "" in
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
		    add db words;
		    let chain = generate db "" in
		       out (make_msg xml chain)
		 end

let rec markov_thread (db, event_channel) =
   begin match Event.sync (Event.receive event_channel) with
      | MMessage (xml, out) ->
	   if not (mem_xml xml ["message"] "subject" []) then
	      let body = try get_cdata xml ~path:["body"] with _ -> "" in
		 if body <> "" then
		    process_groupchat body db xml out
      | MCount (xml, out) ->
	   let result = result_integer db "SELECT COUNT(*) FROM words" in
	      out (make_msg xml (string_of_int result))
      | MTop (xml, out) ->
	   let vm = compile_simple db 
	"SELECT word1, word2, counter FROM words WHERE word1!='' AND word2!='' \
       ORDER BY counter DESC LIMIT 10" in
	   let rec cycle () =
	      try 
		 let data = step_simple vm in
		    (Printf.sprintf "\n%s | %s | %s"
			data.(0) data.(1) data.(2))
		    ^ cycle ()
	      with Sqlite_done -> ""
	   in
	      out (make_msg xml (cycle ()))
      | MStop -> Thread.exit ()
   end;
   markov_thread (db, event_channel)

let get_markov_channel xml =
   let room = get_bare_jid (get_attr_s xml "from") in
      try
	 MarkovMap.find room !markovrooms
      with Not_found ->
	 if GroupchatMap.mem room !groupchats then
	    let event_channel = Event.new_channel () in
	    let db = open_markovdb room in
	       Thread.create markov_thread (db, event_channel);
	       markovrooms := MarkovMap.add room event_channel !markovrooms;
	       event_channel
	 else
	    raise Not_found

let markov_chain xml out =
   try
      let event_channel = get_markov_channel xml in
	 Event.sync (Event.send event_channel (MMessage (xml, out)))
   with _ -> ()

let markov_count text xml out =
   try
      let event_channel = get_markov_channel xml in
	 Event.sync (Event.send event_channel (MCount (xml, out)))
   with _ -> ()

let markov_top text xml out =
   try
      let event_channel = get_markov_channel xml in
	 Event.sync (Event.send event_channel (MTop (xml, out)))
   with _ -> ()
   
let _ =
   register_handle (Catch markov_chain);
   register_handle (Command ("!!!count", markov_count));
   register_handle (Command ("!!!top", markov_top))
      
