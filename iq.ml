(*                                                                          *)
(* (c) 2004, Anastasia Gornostaeva. <ermine@ermine.pp.ru>                   *)
(*                                                                          *)

open Unix
open Xml
open Xmpp
open Hooks

let os = (let f = open_process_in "uname -sr" in
          let answer = input_line f in
             ignore (close_process_in f); answer)

let iq_version_reply xml out =
   out (iq_reply xml
	   [make_simple_cdata "name" "Sulci";
	    make_simple_cdata "version" 
	       (Printf.sprintf "%s (Ocaml %s)" 
		   Version.version Sys.ocaml_version);
	    make_simple_cdata "os" os
	   ])

let iq_query xmlns to_ id =
   Xmlelement ("iq", ["to", to_; "type", "get"; "id", id],
	       [Xmlelement ("query", ["xmlns", xmlns], [])])

(*
iq_last_reply xml =
   iq_reply xml [make_simply_cdata
*)

let _ =
   Hooks.register_handle (Xmlns ("jabber:iq:version", iq_version_reply))
			  (* "jabber:iq:last", Iq.iq_last_reply *)
