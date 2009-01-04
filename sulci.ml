(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xmpp
open Error
open Types
open Config
open Common
open Hooks

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
    try Some (List.assoc "rawxml" Config.logger_options)
    with Not_found -> None
  in
  let run () =
    let jid, out, next_xml = 
      Xmpp.client ~username ~password ~resource ~server ~port 
        ?rawxml_log () in
      
      log#info "Connected to %s!" server;
      
      Sys.set_signal Sys.sigint
        (Sys.Signal_handle (function x -> Hooks.quit out));
      
      Sys.set_signal Sys.sigterm
        (Sys.Signal_handle (function x -> Hooks.quit out));
      
      (* workaround for wildfire *)
      out (make_presence ());
      
      List.iter (fun proc -> 
                   try proc out with exn ->
                     log#error "sulci.ml: %s" (Printexc.to_string exn))
        !on_connect;
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
  let rec reconnect times =
    try
      if times >= 0 then
        run ()
      else
        ()
    with
      | Unix.Unix_error (code, "connect", _) ->
          log#info "Unable to connect to %s:%d: %s"
            server port (Unix.error_message code);
          if times > 0 then (
            Unix.sleep reconnect_interval;
            log#info "Reconnecting. Attempts remains: %d" times;
          );
          reconnect (times - 1)
      | Sasl.Failure cond ->
          log#info "Auth.Failure: %s" cond;
          (match cond with
             | "non-authorized" ->
                 print_endline "will register"
             | _ -> ()
          )
      | Sasl.AuthError reason ->
          log#crit "Authorization failed: %s" reason;
          Pervasives.exit 127
      | Xmpp.XMPPStreamEnd ->
          log#info"The connection to the server is lost";
          List.iter (fun proc -> proc ()) !on_disconnect;
          reconnect count
      | Xmpp.XMPPStreamError els ->
          let cond, text, _ = parse_stream_error els in
            (match cond with
               | `ERR_CONFLICT ->
                   log#info "Connection to the server closed: %s" text
               | _ ->
                   log#info "The server reject us: %s" text
            );
            Pervasives.exit 127
      | exn ->
          log#error "sulci.ml: %s" (Printexc.to_string exn);
          log#error "Probably it is a bug, please send me a bugreport"
  in
    reconnect count
