(*                                                                          *)
(* (c) 2004, 2005, 2006 Anastasia Gornostaeva. <ermine@ermine.pp.ru>        *)
(*                                                                          *)

open Types
open Nicks
open Common
open Config
open Hooks
open Xmpp
open Error

let _ = 
   let server = try 
      trim (Xml.get_cdata config ~path:["jabber"; "server"]) 
   with Not_found ->
      Printf.eprintf "Cannot find servername in config file";
      flush stdout;
      Pervasives.exit 127
   in
   let port = try 
      int_of_string (trim (Xml.get_cdata config ~path:["jabber"; "port"]))
   with Not_found -> 5222 
   in
   let username = try
      trim (Xml.get_cdata config ~path:["jabber"; "user"]) 
   with Not_found ->
      Printf.eprintf "Cannot find username in config file";
      flush stdout;
      Pervasives.exit 127
   in
   let password = try
      trim (Xml.get_cdata config ~path:["jabber"; "password"])
   with Not_found ->
      Printf.eprintf "Cannot find password in config file";
      flush stdout;
      Pervasives.exit 127
   in
   let resource = try
      trim (Xml.get_cdata config ~path:["jabber"; "resource"])
   with Not_found ->
      Printf.eprintf "Cannot find resource name in config file";
      flush stdout;
      Pervasives.exit 127
   in
   let rawxml_log = 
      try Some (trim (Xml.get_cdata config ~path:["log"; "rawxml"]))
      with Not_found -> None 
   in
   let run () =
      let jid, out, next_xml = 
	 Xmpp.client ~username ~password ~resource ~server ~port 
	    ?rawxml_log () in

	 Logger.out (Printf.sprintf "Connected to %s!" server);

	 Sys.set_signal Sys.sigint
	    (Sys.Signal_handle (function x -> Hooks.quit out));
	 
	 Sys.set_signal Sys.sigterm
	    (Sys.Signal_handle (function x -> Hooks.quit out));
      
	 (* workaround for wildfire *)
         out (make_presence ());

	 List.iter (fun proc -> 
		       try proc out with exn -> 
			  Logger.print_exn "sulci.ml" exn) !onstart;
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
	 GroupchatMap.map (fun env -> {env with nicks = [];})
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
	      Logger.out
		 (Printf.sprintf "Unable to connect to %s:%d: %s"
		     server port (Unix.error_message code));
	      if times > 0 then begin
		 Unix.sleep reconnect_interval;
		 Logger.out 
		    (Printf.sprintf "Reconnecting. Attempts remains: %d" 
			times);
	      end;
	      reconnect (times - 1)
	 | Auth.Failure cond ->
	      Logger.out ("Auth.Failure: " ^ cond);
	      (match cond with
		 | "non-authorized" ->
		      print_endline "will register"
		 | _ -> ()
	      )
	 | Auth.AuthError reason ->
	      Logger.out ("Authorization failed: " ^ reason);
	      Pervasives.exit 127
	 | Xmpp.XMPPStreamEnd ->
	      Logger.out "The connection to the server is lost";
	      cleanup_for_reconnect ();
	      reconnect count
	 | Xmpp.XMPPStreamError els ->
	      let cond, text, _ = parse_stream_error els in
		 (match cond with
		     | `ERR_CONFLICT ->
			  Logger.out 
			     (Printf.sprintf 
				 "Connection to the server closed: %s" text)
		     | _ ->
			  Logger.out 
			     (Printf.sprintf 
				 "The server reject us: %s" text)
		 );
		 Pervasives.exit 127
	 | exn ->
	      Logger.print_exn "sulci.ml" exn;
	      Logger.out "Probably it is a bug, please send me a bugreport"
   in
      reconnect count
