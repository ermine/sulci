open Common
open Config
open Hooks
open Xmpp


let _ = 
   let server = trim (Xml.get_cdata config ~path:["jabber"; "server"]) in
   let port = int_of_string (trim (Xml.get_cdata config 
				      ~path:["jabber"; "port"])) in
   let user = trim (Xml.get_cdata config ~path:["jabber"; "user"]) in
   let password = trim (Xml.get_cdata config
			   ~path:["jabber"; "password"]) in
   let resource = trim (Xml.get_cdata config
			   ~path:["jabber"; "resource"]) in
   let debuglog = 
      try trim (Xml.get_cdata config ~path:["debug"; "logfile"]) 
      with Not_found -> "" in

   let jid, out, next_xml = 
      Xmpp.client user password resource ~logfile:debuglog server in

      Sys.set_signal Sys.sigint
	 (Sys.Signal_handle (function x -> Hooks.quit out));

      Sys.set_signal Sys.sigterm
	 (Sys.Signal_handle (function x -> Hooks.quit out));

      List.iter (fun proc -> proc out) !onstart;
      process_xml next_xml out
