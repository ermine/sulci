(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Common
open Xml
open Xmpp
open Types
(*
let muc_handlers = ref []

let register_handle proc =
   muc_handlers := proc :: !muc_handlers
*)

let process_presence room user xml out =
   let room_env = GroupchatMap.find room !groupchats in
   let x = List.find (function 
			 | Xmlelement ("x", attrs, _) ->
			      if (try List.assoc "xmlns" attrs with _ -> "")=
				 "http://jabber.org/protocol/muc#user" 
			      then true else false
			 | _ -> false
		     ) (Xml.get_subels xml) in
      match safe_get_attr_s xml "type"  with
      | "" -> 
	   let status = try get_cdata xml ~path:["status"] with _ -> "" in
	   let show = try get_cdata xml ~path:["show"] with _ -> "available" in
	      if not (Nicks.mem user room_env.nicks) then begin
		 let item = { jid = (try get_attr_s x ~path:["item"] "jid"
				     with _ -> "");
			      role = (try get_attr_s x ~path:["item"] "role" 
				      with _ -> "");
			      affiliation = (try get_attr_s x 
						~path:["item"] "affiliation"
					     with _ -> "");
			      status = status;
			      show = show;
			      orig_nick = user
			    } in
		    groupchats := GroupchatMap.add room 
		       {room_env with nicks = Nicks.add user item
			     room_env.nicks} !groupchats;
		    MUC_join (room, user, item)
	      end
	      else
		 let item = Nicks.find user room_env.nicks in
		 let newitem = {item with status = status; show = show } in
		    groupchats := GroupchatMap.add room 
		       {room_env with 
			   nicks = Nicks.add user newitem
			     room_env.nicks} !groupchats;
		    MUC_presence (room, user, newitem)
      | "unavailable" -> 
	   (match safe_get_attr_s x ~path:["status"] "code" with
	       | "303" -> (* /nick *)
		    let newnick = 
		       get_attr_s xml ~path:["x"; "item"] "nick" in
		    let item = Nicks.find user room_env.nicks in
		       groupchats := GroupchatMap.add room
			  {room_env with nicks = 
				Nicks.add newnick item
				   (Nicks.remove user room_env.nicks)} 
			  !groupchats;
		       MUC_change_nick (room, newnick, user, item)
	       | "307" -> (* /kick *)
		    let item = Nicks.find user 
		       (GroupchatMap.find room !groupchats).nicks in
		    let reason =
		       try get_cdata ~path:["reason"] x with _ -> "" in
		       groupchats := GroupchatMap.add room
			  {room_env with nicks =
				Nicks.remove user room_env.nicks} !groupchats;
		       MUC_kick (room, user, reason, item)
	       | "301" -> (* /ban *)
		    let item = Nicks.find user 
		       (GroupchatMap.find room !groupchats).nicks in
		    let reason = 
		       try get_cdata ~path:["reason"] x with _ -> "" in
		       groupchats := GroupchatMap.add room
			  {room_env with nicks =
				Nicks.remove user room_env.nicks} !groupchats;
		       MUC_ban (room, user, reason, item)
	       | "321" (* non-member *)
	       | _ ->
		    let item = Nicks.find user 
		       (GroupchatMap.find room !groupchats).nicks in
		    let reason = 
		       try get_cdata ~path:["status"] xml with _ -> "" in
		       groupchats := GroupchatMap.add room
			  {room_env with nicks =
				Nicks.remove user room_env.nicks} !groupchats;
		       MUC_leave (room, user, reason, item)
	   )
      | _ -> MUC_other room

let split_nick_body room_env body =
   let rec cycle pos =
      try
	 let colon = String.rindex_from body pos ':' in
	    if String.length body > colon+1 then
	       if body.[colon+1] = ' ' then 
		  let nick = String.sub body 0 colon in
		     if Nicks.mem nick room_env.nicks then
			nick, string_after body (colon+2)
		     else
			cycle (colon-1)
	       else
		  cycle (colon-1)
	    else
	       let nick = String.sub body 0 colon in
		  if Nicks.mem nick room_env.nicks then
		     nick, ""
		  else
		     cycle (colon-1)
      with Not_found ->
	 "", body
   in
      if Nicks.mem body room_env.nicks then
	 body, ""
      else
	 let rn, rt = cycle (String.length body - 1) in
	    if rn = "" then
	       if Nicks.mem rt room_env.nicks then
		  rt, ""
	       else
		  "", rt
	    else
	       rn, rt

let process_message room author xml out = 
   if (mem_xml xml ["message"] "x" ["xmlns", "jabber:x:delay"]) then
      MUC_history room
   else
      try
	 let subject = get_cdata xml ~path:["subject"] in
	    MUC_topic (room, author, subject)
      with Not_found ->
	 try 
	    let body = get_cdata xml ~path:["body"] in
	    let msg_type = 
	       try match get_attr_s xml "type" with
		  | "groupchat" -> `Groupchat
		  | "chat" -> `Chat
		  | _ -> `Normal
	       with _ -> `Normal in
	       match msg_type with
		  | `Groupchat ->
		       let room_env = GroupchatMap.find room !groupchats in
		       let nick, text = split_nick_body room_env body in
			  MUC_message (room, msg_type, author, nick, text)
		  | _ ->
		       MUC_message (room, msg_type, author, "", body)
	 with Not_found ->
	    MUC_other room

(*
let dispatch xml out =
   let from = get_attr_s xml "from" in
   let room = get_bare_jid from in
   let nick = get_resource from in
   let event = 
      if get_tagname xml = "presence" then
	 process_presence room nick xml out
      else
	 if get_tagname xml = "message" then
	    process_message room nick xml out
	 else
	    MUC_other room
   in
      List.iter (fun proc -> proc room event xml out) !muc_handlers
*)

let join_room nick room =
   make_presence 
      ~subels:
      [Xmlelement ("x", ["xmlns", "http://jabber.org/protocol/muc"], [])]
      (room ^ "/" ^ nick)

let kick id room nick (reason, args) =
   let msg = 
      Lang.get_msg ~lang:(GroupchatMap.find room !groupchats).lang reason args
   in
      Xmlelement ("iq", ["to", room; "type", "set"; "id", id],
		  [Xmlelement ("query", ["xmlns",
				      "http://jabber.org/protocol/muc#admin"],
			    [Xmlelement ("item", ["nick", nick; "role", "none"],
					 [make_simple_cdata "reason" msg]
					)])])

let set_topic room subject =
   Xmlelement ("message", ["to", room; "type", "groupchat"],
		[make_simple_cdata "subject" subject])

let register_room nick room =
   groupchats := GroupchatMap.add room {mynick = nick;
			      nicks = Nicks.empty;
			      lang = "ru"} !groupchats
   (* Hooks.register_handle (Hooks.From (room, dispatch)) *)

let _ =
   let default_mynick = 
      trim (Xml.get_cdata Config.config ~path:["jabber"; "user"]) in
   let rconf = 
      try Xml.get_subels ~path:["muc"] ~tag:"room" Config.config with _ -> [] in

      List.iter 
	 (fun r ->
	     let mynick = try Xml.get_attr_s r "nick" with _ -> default_mynick
	     and roomname = Xml.get_attr_s r "jid"
	     and lang = try Xml.get_attr_s r "lang" with _ -> "ru" in
		groupchats:= GroupchatMap.add roomname 
		   {mynick = mynick;
		    nicks = Nicks.empty;
		    lang = lang} !groupchats;
	 ) rconf
 
