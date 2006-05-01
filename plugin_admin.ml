(*                                                                          *)
(* (c) 2004, 2005, 2006 Anastasia Gornostaeva. <ermine@ermine.pp.ru>        *)
(*                                                                          *)

open Common
open Xml
open Xmpp
open Muc
open Hooks
open Types
open Nicks
open Error

let msg text event from xml out =
   if check_access from "admin" then
      let s = String.index text ' ' in
      let to_ = jid_of_string (String.sub text 0 s) in
      let msg_body = string_after text (s+1) in
	 out (Xmlelement ("message", 
			  ["to", to_.string; 
			   "type", 
			   if GroupchatMap.mem (to_.luser, to_.lserver) 
			      !groupchats then
			      "groupchat" else "chat"
			  ],
			  [make_simple_cdata "body" msg_body]))
   else
      make_msg out xml ":-P"
	  
let quit text event from xml out =
   if check_access from "admin" then begin
      make_msg out xml 
	 (Lang.get_msg ~xml "plugin_admin_quit_bye" []);
      Hooks.quit out
   end
   else
      make_msg out xml 
	 (Lang.get_msg ~xml "plugin_admin_quit_no_access" [])

let join_rex = Pcre.regexp "([^\\s]+)(\\s+(.*))?"

let join text event from xml out =
   if check_access from "admin" then
      try
	 let r = Pcre.exec ~rex:join_rex text in
	 let room_s = Pcre.get_substring r 1
	 and nick = try Pcre.get_substring r 3 with Not_found ->
	    trim (get_cdata Config.config ~path:["jabber"; "user"]) in

	 let room = jid_of_string room_s in
	    if not (GroupchatMap.mem (room.luser, room.lserver) !groupchats) 
	    then begin
	       Muc.register_room nick (room.luser, room.lserver);
	       out (Muc.join_room nick (room.luser, room.lserver))
	    end
	    else
	       make_msg out xml "again?"
      with _ ->
	 ()
   else
      make_msg out xml 
	 (Lang.get_msg ~xml "plugin_admin_join_no_access" [])

let leave_rex = Pcre.regexp "([^\\s]+)(\\s+(.*))?"

let leave text event from xml (out:element -> unit) =
   if check_access from "admin" then
      try
	 let r = Pcre.exec ~rex:leave_rex text in
	 let room_s = Pcre.get_substring r 1
	 and reason = 
	    try Some (Pcre.get_substring r 3) with Not_found -> None in
	 let room = jid_of_string room_s in
	    if GroupchatMap.mem (room.luser, room.lserver) !groupchats then
	       out (Muc.leave_room (room.luser, room.lserver) ?reason)
	    else
	       raise Not_found
      with exn ->
	 make_msg out xml "hmm?"


let invite_rex = Pcre.regexp "([^\\s]+)(\\s+(.*))?"

let invite text event from xml (out:element -> unit) =
   if check_access from "admin" then
      try
	 let r = Pcre.exec ~rex:leave_rex text in
	 let who = Pcre.get_substring r 1
	 and room_dst = try Pcre.get_substring r 3 with Not_found -> "" in

	    if room_dst = "" && 
	       GroupchatMap.mem (from.luser, from.lserver) !groupchats then
		  let _ = jid_of_string who in
		     out (Muc.invite (from.luser, from.lserver) who)
	    else
	       let rjid = jid_of_string room_dst in
		  if rjid.lresource = "" &&
		     GroupchatMap.mem (rjid.luser, rjid.lserver) 
		     !groupchats then
			let room_env = GroupchatMap.find
			   (from.luser, from.lserver) !groupchats in
			   try
			      match (Nicks.find who room_env.nicks).jid with
				 | Some j ->
				      out (Muc.invite (rjid.luser, rjid.lserver)
					      (j.luser ^ "@" ^ j.lserver))
				 | None ->
				      raise Not_found
			   with Not_found ->
			      let _ = jid_of_string who in
				 out (Muc.invite (rjid.luser, rjid.lserver) 
					 who)
		  else
		     raise Not_found
      with exn ->
	 make_msg out xml "hmm?"
   
let lang_update text event from xml out =
   if check_access from "admin" then
      if text = "" then
	 make_msg out xml "What language?"
      else
	 make_msg out xml (Lang.update text)


let lr = Pcre.regexp "([a-z][a-z])\\s+(\\w+)(\\s+(.+))?"

let lang_admin text event from xml out =
   if check_access from "admin" then
      if text = "" then
	 make_msg out xml "en msgid string"
      else
	 try
	    let r = Pcre.exec ~rex:lr text in
	    let lang = Pcre.get_substring r 1
	    and msgid = Pcre.get_substring r 2
	    and str = try Some (Pcre.get_substring r 4) with Not_found -> None 
	    in
	       try
		  Lang.update_msgid lang msgid str;
		  make_msg out xml "done"
	       with _ ->
		  make_msg out xml "problems"
	 with _ ->
	    make_msg out xml "invalid syntax"

(* TODO: it is scratch *)

let sulci_set_rex = Pcre.regexp "([a-zA-Z_-]+) *= *(.+)"

let sulci_set text event from xml out =
   if check_access from "admin" then
      try
	 let r = Pcre.exec ~rex:sulci_set_rex text in
	 let var = Pcre.get_substring r 1
	 and value = Pcre.get_substring r 2 in
	    if var = "msg_limit" then
	       try
		  let newvalue = int_of_string value in
		     Common.msg_limit := newvalue
	       with _ ->
		  make_msg out xml "Bad value: must be integer"
	    else
	       make_msg out xml "Unknown variable"
      with Not_found ->
	 make_msg out xml "Hm?"

let _ =
   register_handle (Command ("msg", msg));
   register_handle (Command ("quit", quit));
   register_handle (Command ("join", join));
   register_handle (Command ("leave", leave));
   register_handle (Command ("invite", invite));
   register_handle (Command ("lang_update", lang_update));
   register_handle (Command ("lang_msgid", lang_admin));
   register_handle (Command ("sulci_set", sulci_set));
