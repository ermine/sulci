(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Light_xml
open StreamError
open XMPP
open XMPP.Network
open Types
open Config
open Common
open Hooks
open Lang

let _ = 
  let server = try 
    trim (get_cdata config ~path:["jabber"; "server"]) 
  with Not_found ->
    Printf.eprintf "Cannot find servername in config file";
    Pervasives.flush stdout;
    Pervasives.exit 127
  in
  let port = try 
    int_of_string (trim (get_cdata config ~path:["jabber"; "port"]))
  with Not_found -> 5222 
  in
  let username = try
    trim (get_cdata config ~path:["jabber"; "user"]) 
  with Not_found ->
    Printf.eprintf "Cannot find username in config file";
    Pervasives.flush stdout;
    Pervasives.exit 127
  in
  let password = try
    trim (get_cdata config ~path:["jabber"; "password"])
  with Not_found ->
    Printf.eprintf "Cannot find password in config file";
    Pervasives.flush stdout;
    Pervasives.exit 127
  in
  let resource = try
    trim (get_cdata config ~path:["jabber"; "resource"])
  with Not_found ->
    Printf.eprintf "Cannot find resource name in config file";
    Pervasives.flush stdout;
    Pervasives.exit 127
  in
    (*
  let rawxml_log =
    try Some (List.assoc "rawxml" Config.logger_options)
    with Not_found -> None
  in
    *)
  let run () =
    open_stream_client server port username password resource >>=
      (fun (myjid, p, inch, ouch) ->
         log#info "Connected to %s!" server;
         let out xml = ignore (send ouch (Xmlstream.stanza_serialize p xml)) in
         let () =
           Sys.set_signal Sys.sigint
             (Sys.Signal_handle (function _x -> Hooks.quit out));
      
           Sys.set_signal Sys.sigterm
             (Sys.Signal_handle (function _x -> Hooks.quit out));
         in
           (* workaround for wildfire *)
           out (make_presence ~ns:ns_client ());
           List.iter (fun proc -> try proc out with exn ->
                        log#error "sulci.ml: %s" (Printexc.to_string exn);
                        log#debug "%s" (Printexc.get_backtrace ())
                     ) !on_connect;
           process_xml myjid p inch ouch
      )
  in
    
  let reconnect_interval = 
    try int_of_string (trim (get_attr_s Config.config
                               ~path:["reconnect"] "interval"))
    with Not_found -> 0
  in
  let count =
    try int_of_string (trim (get_attr_s Config.config
                               ~path:["reconnect"] "count"))
    with Not_found -> 0
  in
  let rec reconnect times =
    try
      if times >= 0 then
        run ()
      else
        return ()
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
                 print_endline "will register";
                 return ()
             | _ ->
                 return ()
          )
      | Sasl.AuthError reason ->
          log#crit "Authorization failed: %s" reason;
          Pervasives.exit 127
      | End_of_file ->
          log#info"The connection to the server is lost";
          List.iter (fun proc -> proc ()) !on_disconnect;
          reconnect count
      | StreamError err -> (
          match err.err_condition with
            | ERR_CONFLICT ->
                log#info "Connection to the server closed: %s" err.err_text
            | _ ->
                log#info "The server reject us: %s: %s"
                  (string_of_condition err.err_condition) err.err_text
            );
            Pervasives.exit 127
      | exn ->
          log#error "sulci.ml: %s" (Printexc.to_string exn);
          log#error "Probably it is a bug, please send me a bugreport";
          log#debug "%s" (Printexc.get_backtrace ());
          Pervasives.exit 127
  in
    reconnect count
          
