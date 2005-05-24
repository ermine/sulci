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
		     | Iq `Get ->
			  iq_version_reply "Sulci" Version.version xml out
		     | _ -> ())))
      (* "jabber:iq:last", Iq.iq_last_reply *)
