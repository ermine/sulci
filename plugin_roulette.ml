(*                                                                          *)
(* (c) 2004, Anastasia Gornostaeva. <ermine@ermine.pp.ru>                   *)
(*                                                                          *)

open Xml
open Xmpp
open Common

let r = Random.self_init ()

let roulette text xml out =
   if text <> "" then
      out (make_msg xml 
	      (Lang.get_msg ~xml "plugin_roulette_syntax_error" []))
   else
      if safe_get_attr_s xml "type" = "groupchat" then
	 if Random.int 10 = 1 then
	    let from = get_attr_s xml "from" in
	    let nick = get_resource from in
	    let conf = get_bare_jid from in
	    let id = Hooks.new_id () in
	       out (Muc.kick id conf nick 
		       (Lang.get_msg ~xml "plugin_roulette_kick_reason" []));
	       let proc x o = 
		  let reply = match get_attr_s x "type" with
		     | "result" ->
			  Lang.get_msg ~xml "plugin_roulette_bye" []
		     | "error" ->
			  let err_text =  
			     try 
				get_cdata ~path:["error"; "text"] x 
			     with _ -> 
				Lang.get_msg ~xml "plugin_roulette_kick_failed"
				   []
			  in
			     err_text
		  in
		     o (make_msg xml reply)
	       in
		  Hooks.register_handle (Hooks.Id (id, proc))
	 else
	    out (make_msg xml 
		    (Lang.get_msg ~xml "plugin_roulette_next_time" []))

let _ =
   Hooks.register_handle (Hooks.Command ("tryme", roulette))
