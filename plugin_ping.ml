(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xml
open Xmpp
open Common
open Hooks

(* let seconds_of_float f = Printf.sprintf "%.3g" f *)

let ping text xml (out:element -> unit) =
   let from = Xml.get_attr_s xml "from" in
   let now = Unix.gettimeofday () in
      match safe_get_attr_s xml "type" with
	 | "groupchat" ->
	      let room = get_bare_jid from in
	      let roomenv = GroupchatMap.find room !groupchats in
	      let victim = if text = "" then from else 
		 get_bare_jid from ^ "/" ^ text in
	      let proc x out =
		 let asker = get_resource from in
		 let nick = get_resource victim in
		    match safe_get_attr_s x "type" with
		       | "result" ->
			    let reply =
			       let diff = Unix.gettimeofday () -. now in
				  if nick = roomenv.mynick then
				     Lang.get_msg ~xml
					"plugin_ping_pong_from_me" 
					[Lang.float_seconds ~xml "ping" diff]
				  else
				     if asker = nick then
					Lang.get_msg ~xml
					   "plugin_ping_pong_from_you"
					   [Lang.float_seconds ~xml "ping" diff]
				     else
					Lang.get_msg ~xml
					   "plugin_ping_pong_from_somebody"
					   [nick; 
					    Lang.float_seconds ~xml "ping" diff]
			    in
			       out (make_msg xml reply)
		    | "error" ->
			 let err =
			    try 
			       get_cdata ~path:["error"; "text"] x 
			    with _ ->
			       Lang.get_msg ~xml
				  "plugin_ping_error" [nick]
			 in
			    out (make_msg xml err)
		    | _ -> ()
	      in
	      let id = Hooks.new_id () in
		 Hooks.register_handle (Hooks.Id (id, proc));
		 out (iq_query ~to_:victim ~id "jabber:iq:version")
	 | other ->
	      if text <> "" then
		 out (make_msg xml (Lang.get_msg ~xml
				       "plugin_ping_cannot_pind" [text]))
	      else
		 let proc x out =
		    match get_attr_s x "type" with
		       | "result" ->
			    let diff = Unix.gettimeofday () -. now in
			       out (make_msg xml
				       (Lang.get_msg ~xml 
					   "plugin_ping_pong_from_you"
					   [Lang.float_seconds ~xml "ping" diff]
				       ))
		       | "error" ->
			    let err_text =  
			       try 
				  get_cdata ~path:["error"; "text"] x 
			       with _ -> 
				  Lang.get_msg ~xml "plugin_ping_you_error" []
			    in
			       out (make_msg xml err_text)
		       | _ -> ()
		 in
		 let id = Hooks.new_id () in
		    Hooks.register_handle (Hooks.Id (id, proc));
		    out (iq_query ~id ~to_:from "jabber:iq:version")
		  
let _ =
   register_handle (Command ("ping", ping))
