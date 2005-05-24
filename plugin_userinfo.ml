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

let groupchat from xml out text xmlns myself common =
   let victim = 
      if text = "" then from.lresource 
      else Stringprep.resourceprep text in
   let r = GroupchatMap.find (from.luser, from.lserver) !groupchats in
      if victim = r.mynick then
	 out (make_msg xml myself)
      else
	 let to_ = 
	    if text = "" then string_of_jid from
	    else string_of_jid {from with resource = text} in
	 let proc e f x o =
	    match e with
	       | Iq `Result ->
		    o (make_msg xml (common x from.resource 
					(if text = "" then
					    from.resource else text)))
	       | Iq `Error ->
		    o (make_msg xml (error x))
	       | _ -> ()
	 in
	 let id = new_id () in
	    Hooks.register_handle (Hooks.Id (id, proc));
	    out (iq_query ~to_ ~id xmlns)

let version text event from xml out =
   match event with
      | MUC_message (msg_type, _, _) ->
	   let myself = 
	      Printf.sprintf "Sulci %s - %s" Version.version Jeps.os in
	   let common x author nick =
	      let client = get_cdata x ~path:["query"; "name"] in
	      let version = get_cdata x ~path:["query"; "version"] in
	      let os = get_cdata x ~path:["query"; "os"] in
		 if author = nick then
		    Lang.get_msg ~xml "plugin_userinfo_version_you"
		       [client; version; os]
		 else
		    Lang.get_msg ~xml "plugin_userinfo_version_somebody"
		       [(if text = "" then
			    from.resource else text); client; version; os]
	   in
	      groupchat from xml out text 
		 "jabber:iq:version" myself common
      | _ ->
	   if text <> "" then 
	      out (make_msg xml (Lang.get_msg ~xml 
				    "plugin_userinfo_chat_syntax_error" 
				    [text]));
	   let proc e f x o =
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

let version_server text event from xml out =
   try
      let server = jid_of_string text in
	 if server.user = "" && server.resource = "" then
	    let proc event f x o =
	       match event with
		  | Iq `Result ->
		       let client = get_cdata x ~path:["query"; "name"] in
		       let version = get_cdata x ~path:["query"; "version"] in
		       let os = get_cdata x ~path:["query"; "os"] in
			  o (make_msg xml 
				(Lang.get_msg ~xml 
				    "plugin_userinfo_version_server"
				    [server.server; client; version; os]))
		  | Iq `Error ->
		       o (make_msg xml (error x))
		  | _ -> ()
	    in
	    let id = new_id () in
	       Hooks.register_handle (Hooks.Id (id, proc));
	       out (iq_query ~to_:server.server ~id "jabber:iq:version")
	 else
	    out (make_msg xml "invalid server name")
   with _ ->
      out (make_msg xml "invalid server name")

let idle text event from xml out =
   match event with
      | MUC_message (msg_type, _, _) ->
	   let myself = Lang.get_msg ~xml "plugin_userinfo_idle_me" [] in
	   let common x asker nick =
	      let seconds = get_attr_s x ~path:["query"] "seconds" in
	      let idle = Lang.expand_time ~xml "idle" (int_of_string seconds) in
		 if asker = nick then 
		    Lang.get_msg ~xml "plugin_userinfo_idle_you" [idle]
		 else 
		    Lang.get_msg ~xml "plugin_userinfo_idle_somebody"
		       [(if text = "" then from.resource
			 else text); idle]
	   in
	      groupchat from xml out text "jabber:iq:last" myself common
      | _ ->
	   if text <> "" then 
	      out (make_msg xml (Lang.get_msg ~xml 
				    "plugin_userinfo_chat_syntax_error"
				    [text]));
	   let proc e f x o =
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
		 
let time text event from xml out =
   match event with
      | MUC_message (msg_type, _, _) ->
	   let myself = Lang.get_msg ~xml "plugin_userinfo_time_me"
	      [Strftime.strftime ~tm:(localtime (time ())) "%H:%M"] in
	   let common x asker nick =
	      let display = get_cdata x ~path:["query"; "display"] in
		 if asker = nick then
		    Lang.get_msg ~xml "plugin_userinfo_time_you" [display]
		 else
		    Lang.get_msg ~xml "plugin_userinfo_time_somebody"
		       [(if text = "" then
			    from.resource else text); display]
	   in
	      groupchat from xml out text "jabber:iq:time" myself common
      | _ ->
	   if text <> "" then 
	      out (make_msg xml (Lang.get_msg ~xml 
				    "plugin_userinfo_chat_syntax_error"
				    [text]));
	   let proc e f x o =
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

let status text event from xml out =
   match event with
      | MUC_message (msg_type, _, _) ->
	   let victim = if text = "" then from.lresource else 
	      Stringprep.resourceprep text in
	      begin try
		 let item = Nicks.find victim
		    (GroupchatMap.find (from.luser, from.lserver) 
			!groupchats).nicks in
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
   Hooks.register_handle (Command ("version_server", version_server));
   Hooks.register_handle (Command ("idle", idle));
   Hooks.register_handle (Command ("time", time));
   Hooks.register_handle (Command ("status", status));
