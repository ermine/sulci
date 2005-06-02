(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>             *)
(*                                                                          *)

open Common
open Config
open Hooks
open Xmpp

let _ = 
   let server = trim (Xml.get_cdata config ~path:["jabber"; "server"]) in
   let port = int_of_string (trim (Xml.get_cdata config 
				      ~path:["jabber"; "port"])) in
   let username = trim (Xml.get_cdata config ~path:["jabber"; "user"]) in
   let password = trim (Xml.get_cdata config
			   ~path:["jabber"; "password"]) in
   let resource = trim (Xml.get_cdata config
			   ~path:["jabber"; "resource"]) in
   let logfile = 
      try Some (trim (Xml.get_cdata config ~path:["debug"; "logfile"]))
      with Not_found -> None in

   let jid, out, next_xml = 
      Xmpp.client ~username ~password ~resource ?logfile ~server ~port () in

      Sys.set_signal Sys.sigint
	 (Sys.Signal_handle (function x -> Hooks.quit out));

      Sys.set_signal Sys.sigterm
	 (Sys.Signal_handle (function x -> Hooks.quit out));

      List.iter (fun proc -> try proc out with exn -> print_exn exn) !onstart;
      process_xml next_xml out

