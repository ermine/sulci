(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xml
open Xmpp
open Common
open Unix
open Hooks

let error xml =
   try
      get_error_semantic xml
   with _ -> Lang.get_msg ~xml "plugin_userinfo_error" []

let groupchat xml out text xmlns myself common =
   let from = get_attr_s xml "from" in
   let asker = get_resource from in
   let nick = if text = "" then asker else text in
   let mynick = 
      let r = GroupchatMap.find (get_bare_jid from) !groupchats in r.mynick in
      if nick = mynick then
	 out (make_msg xml myself)
      else
	 let to_ = 
	    if text = "" then from 
	    else get_bare_jid from ^ "/" ^ text in
	 let proc x o =
	    match get_attr_s x "type" with
	       | "result" ->
		    o (make_msg xml (common x asker nick))
	       | "error" ->	
		    o (make_msg xml (error x))
	       | _ -> ()
	 in
	 let id = Hooks.new_id () in
	    Hooks.register_handle (Hooks.Id (id, proc));
	    out (iq_query ~to_ ~id xmlns)

let version text xml out =
   match safe_get_attr_s xml "type" with
      | "groupchat" ->
	   let myself = 
	      Printf.sprintf "Sulci %s - %s" Version.version Jeps.os in
	   let common x asker nick =
	      let client = get_cdata x ~path:["query"; "name"] in
	      let version = get_cdata x ~path:["query"; "version"] in
	      let os = get_cdata x ~path:["query"; "os"] in
		 if asker = nick then
		    Lang.get_msg ~xml "plugin_userinfo_version_you"
		       [client; version; os]
		 else
		    Lang.get_msg ~xml "plugin_userinfo_version_somebody"
		       [nick; client; version; os]
	   in
	      groupchat xml out text "jabber:iq:version" myself common
      | _ ->
	   if text <> "" then 
	      out (make_msg xml (Lang.get_msg ~xml 
				    "plugin_userinfo_chat_syntax_error" 
				    [text]));
	   let proc x o =
	      match get_attr_s x "type" with
		 | "result" ->
		      let client = get_cdata x ~path:["query"; "name"] in
		      let version = get_cdata x ~path:["query"; "version"] in
		      let os = get_cdata x ~path:["query"; "os"] in
			 o (make_msg xml
			       (Lang.get_msg ~xml 
				   "plugin_userinfo_version_you"
				   [client; version; os]))
		 | "error" ->
		      o (make_msg xml (error x))
		 | _ -> ()
	   in
	   let id = Hooks.new_id () in
	      Hooks.register_handle (Hooks.Id (id, proc));
	      out 
		 (iq_query ~to_:(get_attr_s xml "from") ~id "jabber:iq:version")

let idle text xml out =
   match safe_get_attr_s xml "type" with
      | "groupchat" ->
	   let myself = Lang.get_msg ~xml "plugin_userinfo_idle_me" [] in
	   let common x asker nick =
	      let seconds = get_attr_s x ~path:["query"] "seconds" in
	      let idle = seconds_to_text seconds in
		 if asker = nick then 
		    Lang.get_msg ~xml "plugin_userinfo_idle_you" [idle]
		 else 
		    Lang.get_msg ~xml "plugin_userinfo_idle_somebody"
		       [nick; idle]
	   in
	      groupchat xml out text "jabber:iq:last" myself common
      | _ ->
	   if text <> "" then 
	      out (make_msg xml (Lang.get_msg ~xml 
				    "plugin_userinfo_chat_syntax_error"
				    [text]));
	   let proc x o =
	      match get_attr_s x "type" with
		 | "result" ->
		      let seconds = get_attr_s x ~path:["query"] "seconds" in
		      let idle = seconds_to_text seconds in
			 o (make_msg xml 
			       (Lang.get_msg ~xml "plugin_userinfo_idle_you"
				   [idle]))
		 | "error" ->
		      o (make_msg xml (error x))
		 | _ -> ()
	   in
	   let id = Hooks.new_id () in
	      Hooks.register_handle (Hooks.Id (id, proc));
	      out (iq_query ~to_:(get_attr_s xml "from") ~id "jabber:iq:last")
		 
let time text xml out =
   match safe_get_attr_s xml "type" with
      | "groupchat" ->
	   let myself = Lang.get_msg ~xml "plugin_userinfo_time_me"
	      [Strftime.strftime "%H:%M"] in
	   let common x asker nick =
	      let display = get_cdata x ~path:["query"; "display"] in
		 if asker = nick then
		    Lang.get_msg ~xml "plugin_userinfo_time_you" [display]
		 else
		    Lang.get_msg ~xml "plugin_userinfo_time_somebody"
		       [nick; display]
	   in
	      groupchat xml out text "jabber:iq:time" myself common
      | _ ->
	   if text <> "" then 
	      out (make_msg xml (Lang.get_msg ~xml 
				    "plugin_userinfo_chat_syntax_error"
				    [text]));
	   let proc x o =
	      match get_attr_s x "type" with
		 | "result" ->
		      let display = get_cdata x ~path:["query"; "display"] in
			 o (make_msg xml
			       (Lang.get_msg ~xml "plugin_userinfo_time_you"
				   [display]))
		 | "error" ->
		      o (make_msg xml (error x))
		 | _ -> ()
	   in
	   let id = Hooks.new_id () in
	      Hooks.register_handle (Hooks.Id (id, proc));
	      out (iq_query ~id ~to_:(get_attr_s xml "from") "jabber:iq:time")

let status text xml out =
   if safe_get_attr_s xml "type" = "groupchat" then
      let from = get_attr_s xml "from" in
      let room = get_bare_jid from in
      let nick = 
	 if text = "" then
	    get_resource from
	 else
	    text
      in
	 try
	    let item = 
	       Nicks.find nick (GroupchatMap.find room !groupchats).nicks in
	       if item.status = "" then
		  out (make_msg xml ("[" ^ item.show ^ "]"))
	       else
		  out (make_msg xml (item.status ^ " [" ^ item.show ^ "]"))
	 with _ ->
	    out (make_msg xml "Whose status?")

(*
open Sqlite
open Sqlite_util
open Muc

let db =
   let dbf = Sqlite.db_open "./sulci_users.db" in
      if not (result_bool dbf
	"SELECT name FROM SQLITE_MASTER WHERE type='table' AND name='users'")
      then
	 (try exec dbf 
	     "CREATE TABLE users (room varchar, nick varchar, jid varchar,\
last integer, greeting varchar, event varchar, reason varchar)"
	  with Sqlite_error s -> 
	     raise (Failure "error while creating table"));
      dbf

let process_seen room event out =
   match event with
      | MUC_join nick ->
	   let item = 
	      Nicks.find nick (GroupchatMap.find room !groupchats).nicks in
(*	      
	      out (Xmlelement ("message", ["type", "groupchat";
					   "to", room], 
			       [make_simple_cdata "body"
				   (Printf.sprintf "%s (%s) appears" nick item.jid)]))
*)
	      ()
      | MUC_leave (nick, reason) ->
	   let item =
	      Nicks.find nick (GroupchatMap.find room !groupchats).nicks in
	   let jid = tiem.jid in
	   let vm = exec db ("UPDATE ysers SET event= reason") 
	      
      | _ -> ()

let seen text xml out =
   if text = "" then
      out (make_msg xml "who?")
   else
      let reply =
	 let vm = compile_simple db ("SELECT last FROM users WHERE nick=" ^
					escape text) in
	    try 
	       let data = step_simple vm in
		  finalize vm;
		  data.(0)
	    with Sqlite_done -> 
	       "этого еще не встречал"
      in
	 out (make_msg xml reply)
*)

let _ =
   Hooks.register_handle (Command ("version", version));
   Hooks.register_handle (Command ("idle", idle));
   Hooks.register_handle (Command ("time", time));
   Hooks.register_handle (Command ("status", status));
(*
   Hooks.register_handle (Command ("seen", seen));
   Muc.register_handle process_seen
*)
