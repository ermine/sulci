(*
 * (c) 2004-2008 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open Xmpp
open Common
open Types
open Types.Nicks

let r = Random.self_init ()

let roulette text event from xml out =
  if text <> "" then
    make_msg out xml 
	    (Lang.get_msg ~xml "plugin_roulette_syntax_error" [])
  else
    match event with
	    | MUC_message (msg_type, _, _) ->
	        let room_env = GroupchatMap.find (from.luser, from.lserver)
		        !groupchats in
	        let myitem = Nicks.find room_env.mynick room_env.nicks in
		        if myitem.role = `Moderator then
		          let item = Nicks.find from.lresource room_env.nicks in
		            if item.role = `Moderator then
			            make_msg out xml
			              (Lang.get_msg ~xml 
				              "plugin_roulette_not_allowed" [])
		            else if Random.int 10 = 1 then
			            let id = new_id () in
			            let reason = 
			              Lang.get_msg ~xml 
				              "plugin_roulette_kick_reason" [] in
			            let proc e f x o = 
			              match e with
				              | Iq (_, `Result, _) ->
				                  if msg_type = `Groupchat then
					                  make_msg o xml
					                    (Lang.get_msg ~xml 
					                      "plugin_roulette_bye" [])
				              | Iq (_, `Error, _) ->
				                  let err_text =  
					                  try 
					                    get_cdata ~path:["error"; "text"] x 
					                  with _ -> 
					                    Lang.get_msg ~xml 
					                      "plugin_roulette_kick_failed" []
				                  in
					                  make_msg out xml err_text
			            in
			              Hooks.register_handle (Hooks.Id (id, proc));
			              out (Muc.kick ~reason id (from.luser, from.lserver)
				              from.resource);
		            else
			            make_msg out xml (Lang.get_msg ~xml 
					          "plugin_roulette_next_time" [])
		        else
		          make_msg out xml
		            (Lang.get_msg ~xml 
			            "plugin_roulette_not_allowed" [])
                
	    | _ -> ()
          
let _ =
  Hooks.register_handle (Hooks.Command ("tryme", roulette))
