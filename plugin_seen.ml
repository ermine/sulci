(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Sqlite
open Sqlite_util
open Xml
open Xmpp
open Common
open Hooks
open Muc

exception Break

let db =
   let dbf = Sqlite.db_open "./sulci_users.db" in
      if not (result_bool dbf
	"SELECT name FROM SQLITE_MASTER WHERE type='table' AND name='users'")
      then begin
	 try 
	    exec dbf 
	       "CREATE TABLE greeting (jid varchar, room varchar, msg varchar)";
	    exec dbf
	       "CREATE TABLE users (jid varchar, room varchar, nick varchar, last integer, action varchar, reason varchar)"
	 with Sqlite_error s -> 
	    raise (Failure ("error while creating table " ^ s))
      end;
      dbf

let catch_greeting room event out =
   match event with
      | MUC_join (nick, item) ->
	   let jid = get_bare_jid item.jid in
	   let vm = compile_simple db 
	      ("SELECT msg FROM greeting WHERE jid=" ^ escape jid ^
		  " AND room=" ^ escape room) in
	      begin try
		 let result = step_simple vm in
		    finalize vm;
		    let msg = result.(0) in
		       out (Xmlelement ("message", ["to", room;
						    "type", "groupchat"],
					[make_simple_cdata "body"
					    (Printf.sprintf "[%s] %s"nick msg)]
				       ))
	      with _ -> ()
	      end
      | _ -> ()

let add_greet text xml out =
   if check_access (get_attr_s xml "from") "admin" then begin
      if text <> "" then
	 try
	    let s1 = String.index text ' ' in
	    let jid = String.sub text 0 s1 in
	    let s2 = String.index_from text (s1+1) ' ' in
	    let room = String.sub text (s1+1) (s2-s1-1) in
	    let greet = Common.string_after text (s2+1) in
	       if result_bool db ("SELECT msg FROM greeting where jid=" ^
				     escape jid ^ " AND room=" ^ escape room) 
	       then
		  begin
		     exec db ("UPDATE greeting SET msg=" ^ escape greet ^ 
				 " WHERE jid=" ^ escape jid ^ 
				 " AND room=" ^ escape room);
		     out (make_msg xml "Updated")
		  end
	       else
		  begin
		     exec db ("INSERT INTO greeting (jid, room, msg) " ^
				 values [escape jid; 
					 escape room; 
					 escape greet]);
		     out (make_msg xml "Added")
		  end
	 with Not_found ->
	    out (make_msg xml "bad syntax")
   end
   else ()

let catch_seen room event out =
   match event with
      | MUC_leave (nick, reason, item)
      | MUC_kick (nick, reason, item)
      | MUC_ban (nick, reason, item) as action ->
	   let jid = get_bare_jid item.jid in
	   let last = Int32.to_string (Int32.of_float (Unix.time ())) in
	   let action = match action with
	      | MUC_leave _ -> "left"
	      | MUC_kick _ -> "kick"
	      | MUC_ban _ -> "ban"
	   in
	      if result_bool db 
		 ("SELECT last FROM users where jid=" ^ escape jid ^
		     " AND room=" ^ escape room) then
		    exec db ("UPDATE users SET last=" ^ last ^
				", action=" ^ escape action ^
				", reason=" ^ escape reason ^
				" WHERE jid=" ^ escape jid ^ 
				" AND room=" ^ escape room)
	      else
		 exec db 
		   ("INSERT INTO users (jid, room, nick, last, action, reason) "
		    ^ values [escape jid; escape room; escape nick;
			      last; escape action; escape reason])
      | _ -> ()


let find_nick (jid:string) nicks =
   let result = ref [] in
      Nicks.iter (fun nick item ->
		     if get_bare_jid item.jid = jid then
			result := nick :: !result
		 ) nicks;
      if !result = [] then raise Not_found else !result

let verify_nick nick jid nicks xml =
   try
      let item = Nicks.find nick nicks in
	 if jid = get_bare_jid item.jid then
	    Lang.get_msg ~xml "plugin_seen_is_here" [nick]
	 else
	    try let changed = find_nick jid nicks in
	       Lang.get_msg ~xml "plugin_seen_changed_nick" 
		  [nick; (String.concat ", " changed)]
	    with Not_found ->
	       Lang.get_msg ~xml "plugin_seen_is_not_same" [nick; nick]
   with Not_found ->
      let changed = find_nick jid nicks in
	 Lang.get_msg ~xml "plugin_seen_changed_nick" 
	    [nick; (String.concat ", " changed)]

let seen text xml out =
   if text = "" then
      out (make_msg xml "Whom?")
   else
      let reply =
	 try
	    let room = get_bare_jid (get_attr_s xml "from") in
	    let nicks = (GroupchatMap.find room !groupchats).nicks in
	    let vm = compile_simple db 
	       ("SELECT jid, last, action, reason FROM users WHERE nick=" ^ 
		   escape text ^ " AND room=" ^ escape room) in
	       try
		  let result = step_simple vm in
		     try verify_nick text result.(0) nicks xml
		     with Not_found -> begin
			let stamp = float_of_string result.(1) in
			let diff = 
			   Lang.expand_time ~xml "seen"
			      (int_of_float (Unix.time () -. stamp)) in
			   if result.(3) = "" then
			      Lang.get_msg ~xml 
				 (match result.(2) with
				     | "kick" -> "plugin_seen_kicked"
				     | "ban" -> "plugin_seen_banned"
				     | _ -> "plugin_seen_left")
				 [text; diff]
			   else
			      Lang.get_msg ~xml 
				 (match result.(2) with
				     | "kick" -> "plugin_seen_kicked_reason"
				     | "ban" -> "plugin_seen_banned_reason"
				     | _ -> "plugin_seen_left_reason")
				 [text; diff; result.(3)]
		     end
	       with Sqlite_done ->
		  if Nicks.mem text nicks then
		     if get_resource (get_attr_s xml "from") = text then
			Lang.get_msg ~xml "plugin_seen_you" []
		     else
			Lang.get_msg ~xml "plugin_seen_is_here" [text]
		  else begin
		     let result = ref "" in
		     let orig_nick = ref "" in
			begin try
			   Nicks.iter (fun nick item ->
					  if item.orig_nick = text then begin
					     result := nick;
					     orig_nick := item.orig_nick;
					     raise Break
					  end
				      ) nicks;
			with Break -> ()
			end;
			if !result <> "" then
			   if !orig_nick = text then
			      Lang.get_msg ~xml "plugin_seen_you" []
			   else
			      Lang.get_msg ~xml "plugin_seen_changed_nick"
				 [text; !result]
			else
			   Lang.get_msg ~xml "plugin_seen_never_seen" [text]
		  end
	 with Not_found ->
	    Lang.get_msg ~xml "plugin_seen_not_in_room" []
      in
	 out (make_msg xml reply)
			   
let _ =
   Hooks.register_handle (Command ("greet", add_greet));
   Hooks.register_handle (Command ("seen", seen));
   Muc.register_handle catch_greeting;
   Muc.register_handle catch_seen
