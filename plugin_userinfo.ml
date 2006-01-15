(*                                                                          *)
(* (c) 2004, 2005, 2006 Anastasia Gornostaeva. <ermine@ermine.pp.ru>        *)
(*                                                                          *)

open Xml
open Xmpp
open Common
open Unix
open Hooks
open Types
open Nicks
open Muc
open Error

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
	 make_msg out xml myself
      else
	 let to_ = 
	    if text = "" then from.string
	    else string_of_jid {from with resource = text} in
	 let proc e f x o =
	    match e with
	       | Iq (_, `Result, _) ->
		    make_msg o xml (common x from.resource 
				       (if text = "" then
					   from.resource else text))
	       | Iq (_, `Error, _) ->
		    make_msg o xml (error x)
	       | _ -> ()
	 in
	 let id = new_id () in
	    Hooks.register_handle (Hooks.Id (id, proc));
	    out (iq_query ~to_ ~id ~xmlns ~type_:`Get ())

let version text event from xml out =
   match event with
      | MUC_message (msg_type, _, _) ->
	   let myself = 
	      Printf.sprintf "%s %s - %s" Version.name Version.version 
		 Jeps.os in
	   let common x author nick =
	      let client = try get_cdata x ~path:["query"; "name"] with 
		    Not_found -> "[unknown]" in
	      let version = try get_cdata x ~path:["query"; "version"] with
		    Not_found -> "[unknown]" in
	      let os = try get_cdata x ~path:["query"; "os"] with Not_found ->
		 "[unknown]" in
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
	      make_msg out xml (Lang.get_msg ~xml 
				   "plugin_userinfo_chat_syntax_error" 
				   [text]);
	   let proc e f x o =
	      match e with
		 | Iq (_, `Result, _) ->
		      let client = get_cdata x ~path:["query"; "name"] in
		      let version = get_cdata x ~path:["query"; "version"] in
		      let os = get_cdata x ~path:["query"; "os"] in
			 make_msg o xml
			    (Lang.get_msg ~xml 
				"plugin_userinfo_version_you"
				[client; version; os])
		 | Iq (_, `Error, _) ->
		      make_msg o xml (error x)
		 | _ -> ()
	   in
	   let id = new_id () in
	      Hooks.register_handle (Hooks.Id (id, proc));
	      out 
		 (iq_query ~to_:(get_attr_s xml "from") ~id 
		     ~xmlns:"jabber:iq:version" ~type_:`Get ())

let version_server text event from xml out =
   try
      let server = jid_of_string text in
	 if server.user = "" && server.resource = "" then
	    let proc event f x o =
	       match event with
		  | Iq (_, `Result, _) ->
(*
		       let res = String.concat ", "
			  (List.map (fun item ->
					get_tagname item ^ ": " ^
					   get_cdata item
				    ) (get_subels x ~path:["query"])) in
                          make_msg o xml (server.server ^ " -- " ^ res)
*)
		       let client = 
			  try get_cdata x ~path:["query"; "name"] with 
				Not_found -> "n/a" in
		       let version = 
			  try get_cdata x ~path:["query"; "version"] with 
				Not_found -> "n/a" in
		       let os = try get_cdata x ~path:["query"; "os"] with 
			     Not_found -> "n/a" in
			  make_msg o xml 
			     (Lang.get_msg ~xml 
				 "plugin_userinfo_version_server"
				 [server.server; client; version; os])

		  | Iq (_, `Error, _) ->
		       let reply =
			  let cond, type_, _ = parse_error x in
			     match cond with
				| `ERR_FEATURE_NOT_IMPLEMENTED ->
				     Lang.get_msg ~xml
				"plugin_userinfo_version_server_not_implemented"
					[text]
				| `ERR_REMOTE_SERVER_TIMEOUT ->
				     Lang.get_msg ~xml
			 "plugin_userinfo_version_server_remote_server_timeout"
					[text]
				| `ERR_REMOTE_SERVER_NOT_FOUND ->
				     Lang.get_msg ~xml 
			"plugin_userinfo_version_server_remote_server_not_found"
					[text]
				| _ ->
				     Lang.get_msg ~xml 
					"plugin_userinfo_version_server_error" 
					[]
		       in
			  make_msg o xml reply
		  | _ -> ()
	    in
	    let id = new_id () in
	       Hooks.register_handle (Hooks.Id (id, proc));
	       out (iq_query ~to_:server.server ~id 
		       ~xmlns:"jabber:iq:version" ~type_:`Get ())
	 else
	    make_msg out xml "invalid server name"
   with _ ->
      make_msg out xml "invalid server name"

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
	      make_msg out xml (Lang.get_msg ~xml 
				   "plugin_userinfo_chat_syntax_error"
				   [text]);
	   let proc e f x o =
	      match e with
		 | Iq (_, `Result,_) ->
		      let seconds = get_attr_s x ~path:["query"] "seconds" in
		      let idle = 
			 Lang.expand_time ~xml "idle" (int_of_string seconds) in
			 make_msg o xml 
			    (Lang.get_msg ~xml "plugin_userinfo_idle_you"
				[idle])
		 | Iq (_, `Error, _) ->
		      make_msg o xml (error x)
		 | _ -> ()
	   in
	   let id = new_id () in
	      Hooks.register_handle (Hooks.Id (id, proc));
	      out (iq_query ~to_:(get_attr_s xml "from") ~id 
		      ~xmlns:"jabber:iq:last" ~type_:`Get ())
		 
let time text event from xml out =
   match event with
      | MUC_message (msg_type, _, _) ->
	   let myself = Lang.get_msg ~xml "plugin_userinfo_time_me"
	      [Strftime.strftime ~tm:(localtime (gettimeofday ())) "%H:%M"] in
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
	      make_msg out xml (Lang.get_msg ~xml 
				   "plugin_userinfo_chat_syntax_error"
				   [text]);
	   let proc e f x o =
	      match e with
		 | Iq (_, `Result, _) ->
		      let display = get_cdata x ~path:["query"; "display"] in
			 make_msg o xml
			    (Lang.get_msg ~xml "plugin_userinfo_time_you"
				[display])
		 | Iq (_, `Error, _) ->
		      make_msg o xml (error x)
		 | _ -> ()
	   in
	   let id = new_id () in
	      Hooks.register_handle (Hooks.Id (id, proc));
	      out (iq_query ~id ~to_:(get_attr_s xml "from") 
		      ~xmlns:"jabber:iq:time" ~type_:`Get ())

let status text event from xml out =
   match event with
      | MUC_message (msg_type, _, _) ->
	   let victim = if text = "" then from.lresource else 
	      Stringprep.resourceprep text in
	      (try
		  let item = Nicks.find victim
		     (GroupchatMap.find (from.luser, from.lserver) 
			 !groupchats).nicks in
		     make_msg out xml ((if item.status = "" then ""
					else item.status ^ " ") ^
					  "[" ^ (match item.show with
						    | `Online -> "online"
						    | `Away -> "away"
						    | `DND -> "dnd"
						    | `Chat -> "free for chat"
						    | `XA -> "xa") ^ "]")
	       with _ ->
		  make_msg out xml "Whose status?")
      | _ -> ()

let _ =
   Hooks.register_handle (Command ("version", version));
   Hooks.register_handle (Command ("version_server", version_server));
   Hooks.register_handle (Command ("idle", idle));
   Hooks.register_handle (Command ("time", time));
   Hooks.register_handle (Command ("status", status));
