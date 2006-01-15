(*                                                                          *)
(* (c) 2004, 2005, 2006 Anastasia Gornostaeva. <ermine@ermine.pp.ru>        *)
(*                                                                          *)

open Xml
open Xmpp
open Common
open Sqlite
open Sqlite_util
open Hooks
open Types
open Xmpp

type mevent = 
   | MMessage of xmpp_event * jid * Xml.element * (Xml.element -> unit) 
   | MStop 
   | MCount of jid * Xml.element * (Xml.element -> unit)
   | MTop of jid * Xml.element * (Xml.element -> unit)

type t = {
   queue: mevent Queue.t;
   mutex: Mutex.t;
   cond: Condition.t;
}

module MarkovMap = Map.Make(GID)
let markovrooms = ref MarkovMap.empty

let _ = Random.self_init ()

let add_queue (m:t) (mevent:mevent) =
   Mutex.lock m.mutex;
   Queue.add mevent m.queue;
   Condition.signal m.cond;
   Mutex.unlock m.mutex

let take_queue (m:t) =
   Mutex.lock m.mutex;
   while Queue.is_empty m.queue do
      Condition.wait m.cond m.mutex;
   done;
   let e = Queue.take m.queue in
      Mutex.unlock m.mutex;
      e

let open_markovdb (luser, lserver) =
   let path = 
      try trim (Xml.get_attr_s Config.config 
		   ~path:["plugins"; "markov"] "dir")
      with Not_found -> "./markov_db/" in
      if not (Sys.file_exists path) then Unix.mkdir path 0o755;
      let db = Sqlite.db_open (Filename.concat path (luser ^ "@" ^ lserver)) in
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

let add db words =
   let rec cycle1 w1 lst =
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
		 cycle1 w2 tail
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
		    cycle1 w2 tail
	      end
   in
      try
	 cycle1 "" words
      with exn ->
	 Logger.print_exn "Plugin_markov" exn

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
	 let rec cycle2 lsum =
	    let data = step_simple vm in
	       if lsum - int_of_string data.(2) <= 0 then begin
		  finalize vm;
		  data.(0), data.(1)
	       end
	       else
		  cycle2 (lsum - int_of_string data.(2))
	 in
	    try
	       cycle2 (Random.int sum + 1)
	    with 
	       | Sqlite_done -> 
		    w1, ""
	       | exn ->
		    Logger.print_exn "Plugin_markov" exn;
		    w1, ""

let chain_limit = ref
   (try int_of_string (get_attr_s Config.config ~path:["plugin"; "markov"]
			  "msg_limit")
    with Not_found -> 20)

let generate db word =
   let rec cycle3 w i acc =
      if i = !chain_limit then
	 let p = String.concat " " (List.rev acc) in
	    p
      else
	 let w1, w2 = seek db w in
	    if w2 = "" then String.concat " " (List.rev acc)
	    else cycle3 w2 (i+1) (w2::acc)
   in
      try
	 cycle3 word 0 []
      with exn ->
	 Logger.print_exn "Plugin_markov: generate a phrase" exn;
	 ""

let split_words body =
   Pcre.split ~pat:"[ \t\n]+" body

let process_markov db event from xml out =
   match event with
      | MUC_message (msg_type, nick, body) ->
	   let room = from.luser, from.lserver in
	   let room_env = GroupchatMap.find room !groupchats in
	      if from.lresource <> room_env.mynick then
		 let words = split_words body in
		    if words = [] then begin
		       if (msg_type = `Groupchat && nick = room_env.mynick) ||
			  msg_type <> `Groupchat then
			     make_msg out xml "?"
		    end
		    else begin
		       add db words;
		       if (msg_type = `Groupchat && nick = room_env.mynick) ||
			  msg_type <> `Groupchat then
			     let chain = generate db "" in
				make_msg out xml chain
		       else
			  ()
		    end
	      else
		 ()
      | _ -> ()

let rec markov_thread (db, m) =
   begin match take_queue m with
      | MMessage (event, from, xml, out) ->
	   process_markov db event from xml out
      | MCount (from, xml, out) ->
	   (try
	       let result = result_integer db "SELECT COUNT(*) FROM words" in
		  make_msg out xml (string_of_int result)
	    with exn ->
	       Logger.print_exn "Plugin_markov !!!count" exn)
      | MTop (from, xml, out) ->
	   (try
	       let vm = compile_simple db 
		  "SELECT word1, word2, counter FROM words WHERE word1!='' AND word2!='' ORDER BY counter DESC LIMIT 10" in
	       let rec cycle4 () =
		  try 
		     let data = step_simple vm in
			(Printf.sprintf "\n%s | %s | %s"
			    data.(0) data.(1) data.(2))
			^ cycle4 ()
		  with Sqlite_done -> ""
	       in
		  make_msg out xml (cycle4 ())
	    with exn ->
	       Logger.print_exn "Plugin_markov: !!!top" exn)
      | MStop -> Thread.exit ()
   end;
   markov_thread (db, m)

let get_markov_queue room =
   try
      MarkovMap.find room !markovrooms
   with Not_found ->
      let db = open_markovdb room in
      let m = {queue = Queue.create (); mutex = Mutex.create ();
	       cond = Condition.create ()} in
	 ignore (Thread.create markov_thread (db, m));
	 markovrooms := MarkovMap.add room m !markovrooms;
	 m

let markov_chain event from xml out =
   match event with
      | MUC_message _ ->
	   begin try
	      let m = get_markov_queue (from.luser, from.lserver) in
		 add_queue m (MMessage (event, from, xml, out))
	   with _ -> ()
	   end
      | _ -> ()

let markov_count text event from xml out =
   match event with
      | MUC_message _ ->
	   try
	      let m = get_markov_queue (from.luser, from.lserver) in
		 add_queue m (MCount (from, xml, out))
	   with _ -> ()

let markov_top text event from xml out =
   match event with
      | MUC_message _ ->
	   try
	      let m = get_markov_queue (from.luser, from.lserver) in
		 add_queue m (MTop (from, xml, out))
	   with _ -> ()
   
let _ =
   register_handle (Catch markov_chain);
   register_handle (Command ("!!!count", markov_count));
   register_handle (Command ("!!!top", markov_top))
      
