(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xml
open Xmpp
open Common
open Types

let ping text event xml (out:element -> unit) =
   match event with
      | MUC_message (room, msg_type, author, _, _) ->
	   let now = Unix.gettimeofday () in
	      match msg_type with
		 | `Groupchat ->
		      let roomenv = GroupchatMap.find room !groupchats in
		      let victim = if text = "" then author else text in
		      let proc e x out =
			 match e with
			    | Iq `Result ->
				 let reply =
				    let diff = Lang.float_seconds ~xml "ping"
				       (Unix.gettimeofday () -. now) in
				       if victim = roomenv.mynick then
					  Lang.get_msg ~xml 
					     "plugin_ping_pong_from_me" [diff]
				       else
					  if author = victim then
					     Lang.get_msg ~xml
						"plugin_ping_pong_from_you" 
						[diff]
					  else
					     Lang.get_msg ~xml
						"plugin_ping_pong_from_somebody"
						[diff]
				 in
				    out (make_msg xml reply)
			    | Iq `Error ->
				 let err =
				    try 
				       get_cdata ~path:["error"; "text"] x 
				    with _ ->
				       Lang.get_msg ~xml
					  "plugin_ping_error" [victim]
				 in
				    out (make_msg xml err)
			    | _ -> ()
		      in
		      let id = new_id () in
			 Hooks.register_handle (Hooks.Id (id, proc));
			 out (iq_query ~to_:(room ^ "/" ^ victim) ~id 
				 "jabber:iq:version")
		 | other ->
		      if text <> "" then
			 out (make_msg xml 
				 (Lang.get_msg ~xml
				     "plugin_ping_cannot_pind" [text]))
		      else
			 let proc e x out =
			    match e with
			       | Iq `Result ->
				    let diff = Lang.float_seconds ~xml "ping" 
				       (Unix.gettimeofday () -. now) in
				       out (make_msg xml
					       (Lang.get_msg ~xml 
						   "plugin_ping_pong_from_you"
						   [diff]))
			       | Iq `Error ->
				    let err_text =  
				       try 
					  get_cdata ~path:["error"; "text"] x 
				       with _ -> 
					  Lang.get_msg ~xml 
					     "plugin_ping_you_error" []
				    in
				       out (make_msg xml err_text)
			       | _ -> ()
			 in
			 let id = new_id () in
			 let from = get_attr_s xml "from" in
			    Hooks.register_handle (Hooks.Id (id, proc));
			    out (iq_query ~id ~to_:from "jabber:iq:version")
		  
let _ =
   Hooks.register_handle (Hooks.Command ("ping", ping))
