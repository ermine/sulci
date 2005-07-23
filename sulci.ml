(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>             *)
(*                                                                          *)

open Types
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

   let run () =
      let jid, out, next_xml = 
	 Xmpp.client ~username ~password ~resource ?logfile ~server ~port () in

      Sys.set_signal Sys.sigint
	 (Sys.Signal_handle (function x -> Hooks.quit out));

      Sys.set_signal Sys.sigterm
	 (Sys.Signal_handle (function x -> Hooks.quit out));
      
      List.iter (fun proc -> 
		    try proc out with exn -> print_exn exn) !onstart;

      process_xml next_xml out
   in

   let reconnect_interval = 
      try int_of_string (trim (Xml.get_attr_s Config.config
				  ~path:["reconnect"] "interval"))
      with Not_found -> 0
   in
   let count =
      try int_of_string (trim (Xml.get_attr_s Config.config
				  ~path:["reconnect"] "count"))
      with Not_found -> 0
   in
   let cleanup_for_reconnect () =
      (* hooks related *)
      idmap := IdMap.empty;
      xmlnsmap := XmlnsMap.empty;
      presencemap := [];
      (* muc related; clean only nicks *)
      groupchats := 
	 GroupchatMap.map (fun env -> {env with nicks = Nicks.empty;})
	    !groupchats
   in
   let rec reconnect times =
      try
	 if times >= 0 then
	    run ()
	 else
	    ()
      with
	 | Unix.Unix_error (code, "connect", _) ->
	      Printf.printf "Unable to connect to %s:%d: %s\n"
		 server port (Unix.error_message code);
	      flush stdout;
	      if times > 0 then begin
		 Unix.sleep reconnect_interval;
		 Printf.printf "Reconnecting. Attempts remains: %d\n" times;
	      end;
	      reconnect (times - 1)
	 | Auth.AuthError ->
	      print_endline "Authorization failed";
	      flush stdout;
	      Pervasives.exit 127
	 | Xmpp.XMPPStreamEnd ->
	      print_endline "The connection to the server is lost";
	      flush stdout;
	      cleanup_for_reconnect ();
	      reconnect count
	 | Xmpp.XMPPStreamError els ->
	      (* todo: analyze els *)
	      print_endline 
		 "The server reject us because we used invalid protocol.\n";
	      print_endline "Please send me a bugreport";
	      flush stdout;
	      Pervasives.exit 127
	 | exn ->
	      Printf.printf "process_xml exception %s\n"
		 (Printexc.to_string exn);
	      print_endline "Probably it is a bug, please send me a bugreport"
   in
      reconnect count
