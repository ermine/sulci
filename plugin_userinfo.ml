(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xml
open Xmpp
open Common
open Unix
open Hooks
open Types
open Muc

let error xml =
   try
      get_error_semantic xml
   with _ -> 
      Lang.get_msg ~xml "plugin_userinfo_error" []

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
	 let proc e x o =
	    match e with
	       | Iq `Result ->
		    o (make_msg xml (common x asker nick))
	       | Iq `Error ->
		    o (make_msg xml (error x))
	       | _ -> ()
	 in
	 let id = new_id () in
	    Hooks.register_handle (Hooks.Id (id, proc));
	    out (iq_query ~to_ ~id xmlns)

let version text event xml out =
   match event with
      | MUC_message (room, msg_type, author, _, _) ->
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
	   let proc e x o =
	      match e with
		 | Iq `Result ->
		      let client = get_cdata x ~path:["query"; "name"] in
		      let version = get_cdata x ~path:["query"; "version"] in
		      let os = get_cdata x ~path:["query"; "os"] in
			 o (make_msg xml
			       (Lang.get_msg ~xml 
				   "plugin_userinfo_version_you"
				   [client; version; os]))
		 | Iq `Error ->
		      o (make_msg xml (error x))
		 | _ -> ()
	   in
	   let id = new_id () in
	      Hooks.register_handle (Hooks.Id (id, proc));
	      out 
		 (iq_query ~to_:(get_attr_s xml "from") ~id "jabber:iq:version")

let idle text event xml out =
   match event with
      | MUC_message (room, msg_type, author, _, _) ->
	   let myself = Lang.get_msg ~xml "plugin_userinfo_idle_me" [] in
	   let common x asker nick =
	      let seconds = get_attr_s x ~path:["query"] "seconds" in
	      let idle = Lang.expand_time ~xml "idle" (int_of_string seconds) in
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
	   let proc e x o =
	      match e with
		 | Iq `Result ->
		      let seconds = get_attr_s x ~path:["query"] "seconds" in
		      let idle = 
			 Lang.expand_time ~xml "idle" (int_of_string seconds) in
			 o (make_msg xml 
			       (Lang.get_msg ~xml "plugin_userinfo_idle_you"
				   [idle]))
		 | Iq `Error ->
		      o (make_msg xml (error x))
		 | _ -> ()
	   in
	   let id = new_id () in
	      Hooks.register_handle (Hooks.Id (id, proc));
	      out (iq_query ~to_:(get_attr_s xml "from") ~id "jabber:iq:last")
		 
let time text event xml out =
   match event with
      | MUC_message (room, msg_type, author, _, _) ->
	   let myself = Lang.get_msg ~xml "plugin_userinfo_time_me"
	      [Strftime.strftime ~tm:(localtime (time ())) "%H:%M"] in
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
	   let proc e x o =
	      match e with
		 | Iq `Result ->
		      let display = get_cdata x ~path:["query"; "display"] in
			 o (make_msg xml
			       (Lang.get_msg ~xml "plugin_userinfo_time_you"
				   [display]))
		 | Iq `Error ->
		      o (make_msg xml (error x))
		 | _ -> ()
	   in
	   let id = new_id () in
	      Hooks.register_handle (Hooks.Id (id, proc));
	      out (iq_query ~id ~to_:(get_attr_s xml "from") "jabber:iq:time")

let status text event xml out =
   match event with
      | MUC_message (room, msg_type, author, _, _) ->
	   let nick = if text = "" then author else text in
	      begin try
		 let item = Nicks.find nick 
		    (GroupchatMap.find room !groupchats).nicks in
		    if item.status = "" then
		       out (make_msg xml ("[" ^ item.show ^ "]"))
		    else
		       out (make_msg xml (item.status ^ " [" ^ item.show ^ "]"))
	      with _ ->
		 out (make_msg xml "Whose status?")
	      end
      | _ -> ()

let _ =
   Hooks.register_handle (Command ("version", version));
   Hooks.register_handle (Command ("idle", idle));
   Hooks.register_handle (Command ("time", time));
   Hooks.register_handle (Command ("status", status));
