(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Common
open Xml
open Xmpp
open Muc
open Hooks

let check_access jid classname =
   let acls = get_subels Config.config ~tag:"acl" in
      if List.exists (fun a -> 
			 if get_attr_s a "jid" = jid && 
			    get_attr_s a "class" = classname then
			       true else false) acls 
      then true else false


let msg text xml out =
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
	  
let quit text xml out =
   let jid = get_bare_jid (get_attr_s xml "from") in
      if check_access jid "admin" then begin
	 out (make_msg xml "Пока!");
	 Hooks.quit out
      end
      else
	 out (make_msg xml "Не хочу!")

let join text xml out =
   let jid = get_bare_jid (get_attr_s xml "from") in
      if check_access jid "admin" then
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
	 out (make_msg xml "Не хочу!")
   
let lang_update text xml out =
   let jid = get_bare_jid (get_attr_s xml "from") in
      if check_access jid "admin" then
	 if text = "" then
	    out (make_msg xml "What language?")
	 else
	    out (make_msg xml (Lang.update text))

let _ =
   register_handle (Command ("msg", msg));
   register_handle (Command ("quit", quit));
   register_handle (Command ("join", join));
   register_handle (Command ("lang_update", lang_update))
