(*                                                                          *)
(* (c) 2004, 2005, 2006 Anastasia Gornostaeva. <ermine@ermine.pp.ru>        *)
(*                                                                          *)

open Xml
open Xmpp
open Common
open Types
open Xmpp

let success starttime lang victim lvictim mynick from xml out =
   let diff = Lang.float_seconds ~lang "ping" 
      (Unix.gettimeofday () -. starttime) in
      if lvictim = mynick then
	 Lang.get_msg ~lang "plugin_ping_pong_from_me" [diff]
      else
	 if from.lresource = lvictim then
	    Lang.get_msg ~lang "plugin_ping_pong_from_you" [diff]
	 else
	    Lang.get_msg ~lang "plugin_ping_pong_from_somebody"[victim; diff]
	

let ping text event from xml (out:element -> unit) =
   match event with
      | MUC_message (`Groupchat, _, _) ->
	   let now = Unix.gettimeofday () in
	   let roomenv = GroupchatMap.find (from.luser, from.lserver)
	      !groupchats in
	   let lang = roomenv.lang in
	   let lvictim = if text = "" then from.lresource
	   else Stringprep.resourceprep text in
	   let victim = if text = "" then from.resource else text in
	   let proc e f x o =
	      let reply =
		 match e with
		    | Iq (_, `Result, _) ->
			 success now lang victim lvictim 
			    roomenv.mynick from  x o
		    | Iq (_, `Error, _) ->
			 (let cond,_,_,_ = Error.parse_error x in
			     match cond with
				| `ERR_FEATURE_NOT_IMPLEMENTED ->
				     success now lang victim
					lvictim roomenv.mynick
					from x o
				| _ ->
				     try get_cdata 
					~path:["error"; "text"] x 
				     with _ ->
					Lang.get_msg ~lang
					   "plugin_ping_error"
					   [victim]
			 )
		    | _ -> "?!"
	      in
		 make_msg out xml reply
	   in
	   let id = new_id () in
	      Hooks.register_handle (Hooks.Id (id, proc));
	      out (iq_query ~to_:(string_of_jid {from with resource = victim})
		      ~xmlns:"jabber:iq:version" ~id ~type_:`Get ())
      | MUC_message _
      | Message ->
	   let now = Unix.gettimeofday () in
	      if text <> "" then
		 make_msg out xml (Lang.get_msg ~xml
				      "plugin_ping_cannot_ping" [text])
	      else
		 let proc e f x out =
		    match e with
		       | Iq (_, `Result, _) ->
			    let diff = Lang.float_seconds ~xml "ping" 
			       (Unix.gettimeofday () -. now) in
			       make_msg out xml
				  (Lang.get_msg ~xml 
				      "plugin_ping_pong_from_you" [diff])
		       | Iq (_, `Error,_) ->
			    let err_text =  
			       try 
				  get_cdata ~path:["error"; "text"] x 
			       with _ -> 
				  Lang.get_msg ~xml 
				     "plugin_ping_you_error" []
			    in
			       make_msg out xml err_text
		       | _ -> ()
		 in
		 let id = new_id () in
		 let from = get_attr_s xml "from" in
		    Hooks.register_handle (Hooks.Id (id, proc));
		    out (iq_query ~xmlns:"jabber:iq:version" ~id ~to_:from 
			    ~type_:`Get ())
      | _ -> ()
		  
let _ =
   Hooks.register_handle (Hooks.Command ("ping", ping))
