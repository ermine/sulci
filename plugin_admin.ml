(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Common
open Xml
open Xmpp
open Muc
open Hooks

let check_access jid classname =
   let who =
      let room = get_bare_jid jid in
	 try 
	    let env = GroupchatMap.find room !groupchats in
	    let nick = get_resource jid in
	    let item = Nicks.find nick env.nicks in
	       get_bare_jid (item.jid)
	 with Not_found -> get_bare_jid jid 
   in
   let acls = get_subels Config.config ~tag:"acl" in
      if List.exists (fun a -> 
			 if get_attr_s a "jid" = who &&
			    get_attr_s a "class" = classname then
			       true else false) acls 
      then true else false

let msg text xml out =
   if check_access (get_attr_s xml "from") "admin" then
      let s = String.index text ' ' in
      let to_ = String.sub text 0 s in
      let msg_body = string_after text (s+1) in
	 out (Xmlelement ("message", 
			  ["to", to_; 
			   "type", 
			   try
			      let _ = GroupchatMap.find to_ !groupchats in
				 "groupchat"
			   with Not_found -> "chat"
			  ],
			  [make_simple_cdata "body" msg_body]))
   else
      out (make_msg xml ":-P")
	  
let quit text xml out =
   if check_access (get_attr_s xml "from") "admin" then begin
      out (make_msg xml 
	      (Lang.get_msg ~xml "plugin_admin_quit_bye" []));
      Hooks.quit out
   end
   else
      out (make_msg xml 
	      (Lang.get_msg ~xml "plugin_admin_quit_no_access" []))

let join text xml out =
   if check_access (get_attr_s xml "from") "admin" then
      let room, nick =
	 try
	    let s = String.index text ' ' in
	    let room = String.sub text 0 s in
	    let nick = string_after text (s+1) in
	       room, nick
	 with Not_found ->
	    text,
	    trim (get_cdata Config.config ~path:["jabber"; "user"])
      in
	 Muc.register_room nick room;
	 out (Muc.join_room nick room)
   else
      out (make_msg xml 
	      (Lang.get_msg ~xml "plugin_admin_join_no_access" []))
	 
let lang_update text xml out =
   if check_access (get_attr_s xml "from") "admin" then
      if text = "" then
	 out (make_msg xml "What language?")
      else
	 out (make_msg xml (Lang.update text))

let _ =
   register_handle (Command ("msg", msg));
   register_handle (Command ("quit", quit));
   register_handle (Command ("join", join));
   register_handle (Command ("lang_update", lang_update))
