(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Jeps
open Hooks
open Types

let _ =
   Hooks.register_handle 
      (Xmlns ("jabber:iq:version", 
	      (fun event from xml out -> 
		  match event with
		     | Iq (id, type_, xmlns) ->
			  if type_ = `Get && xmlns = "jabber:iq:version" then
			     out (iq_version_reply 
				     Version.name Version.version xml)
			  else
			     ()
		     | _ -> ())))
      (* "jabber:iq:last", Iq.iq_last_reply *)
