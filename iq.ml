open Unix
open Xml
open Xmpp
open Types

let os = (let f = open_process_in "uname -sr" in
          let answer = input_line f in
             ignore (close_process_in f); answer)

let iq_version_reply out xml =
   out (iq_reply xml
	   [make_simple_cdata "name" "Stoat";
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
