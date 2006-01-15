(*                                                                          *)
(* (c) 2004, 2005, 2006 Anastasia Gornostaeva. <ermine@ermine.pp.ru>        *)
(*                                                                          *)

open Xml
open Xmpp
open Common
open Types

let r = Random.self_init ()

let roulette text event from xml out =
   if text <> "" then
      make_msg out xml 
	 (Lang.get_msg ~xml "plugin_roulette_syntax_error" [])
   else
      match event with
	 | MUC_message (msg_type, _, _) when msg_type = `Groupchat ->
	      if Random.int 10 = 1 then
		 let id = new_id () in
		    out (Muc.kick id from from.resource 
			    ("plugin_roulette_kick_reason", []));
		    let proc e f x o = 
		       let reply = match e with
			  | Iq (_, `Result, _) ->
			       Lang.get_msg ~xml "plugin_roulette_bye" []
			  | Iq (_, `Error, _) ->
			       let err_text =  
				  try 
				     get_cdata ~path:["error"; "text"] x 
				  with _ -> 
				     Lang.get_msg ~xml 
					"plugin_roulette_kick_failed" []
			       in
				  err_text
		       in
			  make_msg o xml reply
		    in
		       Hooks.register_handle (Hooks.Id (id, proc))
	      else
		 make_msg out xml (Lang.get_msg ~xml 
				      "plugin_roulette_next_time" [])
	 | _ -> ()

let _ =
   Hooks.register_handle (Hooks.Command ("tryme", roulette))
