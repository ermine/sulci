(*
 * (c) 2004-2008  Anastasia Gornostaeva. <ermine@ermine.pp.ru>
*)

open Xml
open Xmpp
open Common
open Types
open Nicks
open Sqlite
open Sqlite_util

let total = ref 0

let db =
  let file = 
    try trim (Xml.get_attr_s Config.config 
      ~path:["plugins"; "vocabulary"] "db")
    with Not_found -> "wtf.db" in
  let db = Sqlite.db_open file in
    if not (result_bool db
      "SELECT name FROM SQLITE_MASTER WHERE type='table' AND name='wtf'")
    then begin try
      exec db
        "CREATE TABLE wtf (stamp integer, nick varchar, luser varchar, lserver varchar, key varchar, value varchar)";
      exec db "create index dfnidx on wtf(key)";
    with Sqlite_error s -> 
      raise (Failure "error while creating table")
    end;
    let () =
	    let vm = compile_simple db "SELECT count(*) FROM wtf" in
	      try let result = step_simple vm in
	        finalize vm;
	        total := int_of_string result.(0)
	      with Sqlite_done ->
	        total := 0
    in
	    db

let dfn text event from xml out =
  try
    let eq = String.index text '=' in
    let key = 
	    let key' = trim (String.sub text 0 eq) in
	      if key' = "" then raise Not_found
	      else
	        key'
    in
    let value = trim (string_after text (eq+1)) in
    let nick, luser, lserver, cond =
	    match event with
	      | MUC_message _ ->
		        let room = (from.luser, from.lserver) in
		        let nicks = (GroupchatMap.find room !groupchats).nicks in
		          (match (Nicks.find from.lresource nicks).jid with
			          | Some jid ->
			              let cond = " AND luser=" ^ escape jid.luser ^
				              " AND lserver=" ^ escape jid.lserver in
				              from.lresource, jid.luser, jid.lserver, cond
			          | None ->
			              let cond = " AND nick=" ^ escape from.lresource ^
				              " AND luser=" ^ escape from.luser ^
				              " AND lserver=" ^ escape from.lserver in
				              from.lresource, from.luser, from.lserver, cond
		          )
	      | _ ->
		        let cond = " AND luser=" ^ escape from.luser ^ 
		          " AND lserver=" ^ escape from.lserver in
		          from.luser, from.luser, from.lserver, cond
    in
    let sql = ("SELECT value FROM wtf WHERE key=" ^ escape key ^ cond) in
    let vm = compile_simple db 
	    ("SELECT value FROM wtf WHERE key=" ^ escape key ^ cond) in
	    try
	      let result = step_simple vm in
	        finalize vm;
	        if value = result.(0) then
		        make_msg out xml 
		          (Lang.get_msg ~xml "plugin_vocabulary_dfn_again" [])
	        else if value = "" then begin
		        exec db ("DELETE FROM wtf WHERE key=" ^ escape key ^ cond);
		        decr total;
		        make_msg out xml 
		          (Lang.get_msg ~xml "plugin_vocabulary_removed" [])
	        end
	        else
		        let stamp = Int32.to_string (Int32.of_float 
						  (Unix.gettimeofday ())) in
		          exec db ("UPDATE wtf SET stamp=" ^ stamp ^
				        ", nick=" ^ escape nick ^
				        ", value=" ^ escape value ^
				        " WHERE key=" ^ escape key ^ cond);
		          make_msg out xml 
			          (Lang.get_msg ~xml "plugin_vocabulary_replaced" [])
	    with Sqlite_done ->
	      if value <> "" then
	        let stamp = Int32.to_string (Int32.of_float 
					  (Unix.gettimeofday ())) in
		        exec db 
		          ("INSERT INTO wtf (stamp,nick,luser,lserver,key,value) " ^
			          values [stamp; 
				        escape nick; 
				        escape luser; escape lserver;
				        escape key; escape value]);
		        incr total;
		        make_msg out xml 
		          (Lang.get_msg ~xml "plugin_vocabulary_recorded" [])
	      else
	        make_msg out xml
		        (Lang.get_msg ~xml "plugin_vocabulary_nothing_to_remove" [])
  with Not_found ->
    make_msg out xml 
      (Lang.get_msg ~xml "plugin_vocabulary_invalid_syntax" [])
      
let wtf text event from xml out =
  if text = "" then
    make_msg out xml 
	    (Lang.get_msg ~xml "plugin_vocabulary_invalid_syntax" [])
  else
    let key =
	    try
	      let q = String.index text '?' in
	        String.sub text 0 q
	    with Not_found -> text in
    let vm = compile_simple db 
	    ("SELECT nick, value FROM wtf WHERE key=" ^ escape key ^
	      " ORDER BY stamp DESC LIMIT 1") in
	    try
	      let result = step_simple vm in
	        finalize vm;
	        make_msg out xml 
		        (Lang.get_msg ~xml "plugin_vocabulary_answer"
		          [result.(0); key; result.(1)])
	    with Sqlite_done ->
	      make_msg out xml 
	        (Lang.get_msg ~xml "plugin_vocabulary_not_found" [])
          
let wtfall text event from xml out =
  if text = "" then
    make_msg out xml 
	    (Lang.get_msg ~xml "plugin_vocabulary_invalid_syntax" [])
  else
    let key =
	    try
	      let q = String.index text '?' in
	        String.sub text 0 q
	    with Not_found -> text in
    let vm = compile_simple db 
	    ("SELECT nick, value FROM wtf WHERE key=" ^ escape key ^
	      " ORDER BY stamp") in
    let rec aux_acc i acc =
	    let result = try Some (step_simple vm) with Sqlite_done -> None in
	      match result with
	        | Some r ->
		          let str = 
		            string_of_int i ^ ") " ^
			            Lang.get_msg ~xml "plugin_vocabulary_answer"
			            [r.(0); key; r.(1)] in
		            aux_acc (i+1) (str :: acc)
	        | None -> 
		          if acc = [] then "" else
		            String.concat "\n" (List.rev acc)
    in
    let reply = aux_acc 1 [] in
	    if reply = "" then
	      make_msg out xml 
	        (Lang.get_msg ~xml "plugin_vocabulary_not_found" [])
	    else
	      make_msg out xml reply
          
let wtfrand text event from xml out =
  let key = trim(text) in
    if key = "" then
	    let rand = string_of_int (Random.int (!total)) in
	    let vm = compile_simple db
	      ("SELECT nick, key, value FROM wtf LIMIT " ^ rand ^ ",1") in
	      try 
	        let r = step_simple vm in 
		        finalize vm;
		        make_msg out xml 
		          (Lang.get_msg ~xml "plugin_vocabulary_answer"
			          [r.(0); r.(1); r.(2)])
	      with Sqlite_done ->
	        make_msg out xml 
		        (Lang.get_msg ~xml "plugin_vocabulary_db_is_empty" [])
    else
	    let vm = compile_simple db ("SELECT count(*) FROM wtf WHERE key=" ^
				escape key) in
	      try 
	        let r = step_simple vm in
		        finalize vm;
		        let vm' = compile_simple db 
		          ("SELECT nick, value FROM wtf WHERE key=" ^ escape key ^
			          " LIMIT " ^ string_of_int 
			          (Random.int (int_of_string r.(0))) ^ ",1") in
		        let r' = step_simple vm' in
		          finalize vm';
		          make_msg out xml 
			          (Lang.get_msg ~xml "plugin_vocabulary_answer"
			            [r'.(0); key; r'.(1)])
	      with Sqlite_done ->
	        make_msg out xml 
		        (Lang.get_msg ~xml "plugin_vocabulary_not_found" [])
            
let wtfcount text event from xml out =
  let key = trim(text) in
  let vm = compile_simple db 
    (if key = "" then
	    "SELECT count(*) FROM wtf"
    else
	    "SELECT count(*) FROM wtf WHERE key=" ^ escape key)
  in
    try
	    let r = step_simple vm in
	      finalize vm;
	      if key = "" then
	        total := int_of_string r.(0);
	      make_msg out xml 
	        (Lang.get_msg ~xml "plugin_vocabulary_records" [r.(0)])
    with Sqlite_done ->
	    make_msg out xml 
	      (Lang.get_msg ~xml "plugin_vocabulary_db_is_empty" [])
        
let _ =
  Hooks.register_handle (Hooks.Command ("wtf", wtf));
  Hooks.register_handle (Hooks.Command ("wtfall", wtfall));
  Hooks.register_handle (Hooks.Command ("wtfrand", wtfrand));
  Hooks.register_handle (Hooks.Command ("wtfcount", wtfcount));
  Hooks.register_handle (Hooks.Command ("dfn", dfn));
  
