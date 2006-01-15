(*                                                                          *)
(* (c) 2004, 2005, 2006 Anastasia Gornostaeva. <ermine@ermine.pp.ru>        *)
(*                                                                          *)

open Sqlite
open Sqlite_util
open Xml
open Xmpp
open Common
open Hooks
open Types
open Nicks

exception Break

let db =
   let dbf = Sqlite.db_open "./sulci_users.db" in
      if not (result_bool dbf
	"SELECT name FROM SQLITE_MASTER WHERE type='table' AND name='users'")
      then begin
	 try 
	    exec dbf 
	       "CREATE TABLE greeting (jid varchar, room varchar, msg varchar)";
	    exec dbf "CREATE INDEX gr_index on greeting (jid, room)";
	    exec dbf
	       "CREATE TABLE users (jid varchar, room varchar, nick varchar, last integer, action varchar, reason varchar)";
	    exec dbf "CREATE INDEX users_index on users(jid, room)";
	 with Sqlite_error s -> 
	    raise (Failure ("error while creating table " ^ s))
      end;
      dbf

let add_greet text event from xml out =
   if check_access from "admin" then begin
      if text <> "" then
	 try
	    let s1 = String.index text ' ' in
	    let jid = jid_of_string (String.sub text 0 s1) in
	    let jid_s = jid.luser ^ "@" ^ jid.lserver in
	    let s2 = String.index_from text (s1+1) ' ' in
	    let room = jid_of_string (String.sub text (s1+1) (s2-s1-1)) in
	    let room_s = room.luser ^ "@" ^ room.lserver in
	    let greet = Common.string_after text (s2+1) in
	       if result_bool db ("SELECT msg FROM greeting where jid=" ^
				     escape jid_s ^
				     " AND room=" ^ escape room_s)
	       then
		  begin
		     exec db ("UPDATE greeting SET msg=" ^ escape greet ^ 
				 " WHERE jid=" ^ escape jid_s ^
				 " AND room=" ^ escape room_s);
		     make_msg out xml
			(Lang.get_msg ~xml "plugin_seen_greet_updated" [])
		  end
	       else
		  begin
		     exec db ("INSERT INTO greeting (jid, room, msg) " ^
				 values [escape jid_s; 
					 escape room_s; 
					 escape greet]);
		     make_msg out xml
			(Lang.get_msg ~xml "plugin_seen_greet_added" [])
		  end
	 with Not_found ->
	    make_msg out xml 
	       (Lang.get_msg ~xml "plugin_seen_greet_bad_syntax" [])
   end
   else ()

let catch_seen event from xml out =
   let room_s = from.luser ^ "@" ^ from.lserver in
      match event with
	 | MUC_join item ->
	      let jid_s =
		 match item.jid with
		    | None -> ""
		    | Some j -> j.luser ^ "@" ^ j.lserver
	      in
	      let vm = compile_simple db 
		 ("SELECT msg FROM greeting WHERE jid=" ^ escape jid_s ^
		     " AND room=" ^ escape room_s) in
		 begin try
		    let result = step_simple vm in
		       finalize vm;
		       let msg = result.(0) in
			  out (Xmlelement ("message", ["to", room_s;
						       "type", "groupchat"],
					   [make_simple_cdata "body"
					       (Printf.sprintf "[%s] %s"
						   from.resource msg)]
				       ))
		 with _ -> ()
		 end
	 | MUC_leave (reason, item)
	 | MUC_kick (reason, item)
	 | MUC_ban (reason, item) as action ->
	      let cond =
		 match item.jid with
		    | None -> 
			 "nick=" ^ escape from.lresource
		    | Some j -> 
			 "jid=" ^ escape (j.luser ^ "@" ^ j.lserver)
	      in
	      let room_s = from.luser ^ "@" ^ from.lserver in
	      let last = Int32.to_string (Int32.of_float 
					     (Unix.gettimeofday ())) in
	      let action = match action with
		 | MUC_leave _ -> "left"
		 | MUC_kick _ -> "kick"
		 | MUC_ban _ -> "ban"
	      in
		 if result_bool db 
		    ("SELECT last FROM users where " ^ cond ^
			" AND room=" ^ escape room_s) then
		       exec db ("UPDATE users SET last=" ^ last ^
				   ", action=" ^ escape action ^
				   ", reason=" ^ escape reason ^
				   " WHERE " ^ cond ^
				   " AND room=" ^ escape room_s)
		 else
		    exec db 
		   ("INSERT INTO users (jid, room, nick, last, action, reason) "
		    ^ values [escape (match item.jid with
					 | None -> ""
					 | Some j -> j.luser ^ "@" ^ j.lserver);
			      escape room_s; 
			      escape from.resource;
			      last; escape action; escape reason])
	 | _ -> ()


let find_nick (jid:string) nicks =
   let result = ref [] in
      Nicks.iter (fun (nick, item) ->
		     match item.jid with
			| None -> ()
			| Some j ->
			     if jid = j.luser ^ "@" ^ j.lserver then
				result := nick :: !result
		 ) nicks;
      if !result = [] then raise Not_found else !result

let verify_nick nick jid nicks xml =
   try
      let item = Nicks.find nick nicks in
	 match item.jid with
	    | None ->
		 Lang.get_msg ~xml "plugin_seen_is_here" [nick]
	    | Some j ->
		 if jid = j.luser ^ "@" ^ j.lserver then
		    Lang.get_msg ~xml "plugin_seen_is_here" [nick]
		 else if jid = "" then
		    Lang.get_msg ~xml "plugin_seen_is_here" [nick]
		 else
		    try let changed = find_nick jid nicks in
		       Lang.get_msg ~xml "plugin_seen_changed_nick" 
			  [nick; (String.concat ", " changed)]
		    with Not_found ->
		       Lang.get_msg ~xml "plugin_seen_is_not_same" [nick; nick]
   with Not_found ->
      if jid <> "" then
	 let changed = find_nick jid nicks in
	    Lang.get_msg ~xml "plugin_seen_changed_nick" 
	       [nick; (String.concat ", " changed)]
      else
	 raise Not_found

let seen text event from xml out =
   if text = "" then
      make_msg out xml (Lang.get_msg ~xml "plugin_seen_whom" [])
   else
      match event with
	 | MUC_message (msg_type, _, _) ->
	      let room  = from.luser, from.lserver in
	      let nicks = (GroupchatMap.find room !groupchats).nicks in
	      let vm = compile_simple db 
		 ("SELECT jid, last, action, reason FROM users WHERE nick=" ^
		     escape text ^ " AND room=" ^ escape 
		     (from.luser ^ "@" ^ from.lserver) ^ 
		     " ORDER BY last DESC LIMIT 1") in
	      let reply = try
		 let result = step_simple vm in
		    finalize vm;
		    try verify_nick text result.(0) nicks xml
		    with Not_found -> begin
		       let stamp = float_of_string result.(1) in
		       let diff = 
			  Lang.expand_time ~xml "seen"
			    (int_of_float (Unix.gettimeofday () -. stamp)) in
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
				    | "kick" -> 
					 "plugin_seen_kicked_reason"
				    | "ban" -> 
					 "plugin_seen_banned_reason"
				    | _ -> 
					 "plugin_seen_left_reason")
				[text; diff; result.(3)]
		    end
	      with Sqlite_done -> begin
		 if Nicks.mem text nicks then
		    if get_resource (get_attr_s xml "from") = text then
		       Lang.get_msg ~xml "plugin_seen_you" []
		    else
		       Lang.get_msg ~xml "plugin_seen_is_here" [text]
		 else begin
		    let result = ref "" in
		    let orig_nick = ref "" in
		       begin try
			  Nicks.iter (fun (nick, item) ->
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
	      end
	      in
		 make_msg out xml reply
	 | _ ->
	      make_msg out xml (Lang.get_msg ~xml "plugin_seen_not_in_room" [])

			   
let _ =
   Hooks.register_handle (Command ("greet", add_greet));
   Hooks.register_handle (Command ("seen", seen));
   Hooks.register_handle (Catch catch_seen)
