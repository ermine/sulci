(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Light_xml
open Transport
open StreamError
open XMPP
open Jid
open Hooks
open Config
  
let session xmpp =
  log#info "Connected to %s!" xmpp.myjid.domain;

  XMPP.register_stanza_handler xmpp (ns_client, "message")
    (XMPP.parse_message ~callback:message_callback
       ~callback_error:message_error);
  XMPP.register_stanza_handler xmpp (ns_client, "presence")
    (XMPP.parse_presence ~callback:presence_callback
       ~callback_error:presence_error);
  
  Iq.features xmpp;
    
  (* workaround for wildfire *)
  send_presence xmpp ();

  List.iter (fun proc -> try proc xmpp with exn ->
               log#error "sulci.ml: %s" (Printexc.to_string exn);
               log#debug "%s" (Printexc.get_backtrace ())
            ) global.on_connect

let run account =
  let myjid =
    if account.resource = "" then
      account.jid
    else
      replace_resource account.jid account.resource
  in
  let session_key = string_of_int (Random.int 1000) in
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let socket = {
    fd = s;
    send = send s;
    read = read s
  } in
  let host, port =
    (if account.ip = "" then account.jid.domain else account.ip),
    (match account.port with
       | None -> 5222
       | Some i -> i
    )
  in
  let xmpp = XMPP.create session_key socket myjid in
    Transport.connect s host port;
    XMPP.open_stream xmpp ~use_tls:false account.password session;
    let rec loop () =
      XMPP.parse xmpp;
      loop ()
    in
      loop ()

let rec reconnect account times =
  try
    if times >= 0 then
      run account
  with
(*      
    | Unix.Unix_error (code, "connect", _) ->
        log#info "Unable to connect to %s:%d: %s"
          host port (Unix.error_message code);
        if times > 0 then (
          Unix.sleep reconnect_interval;
          log#info "Reconnecting. Attempts remains: %d" times;
        );
        reconnect (times - 1)
*)        
    | Sasl.Failure cond ->
        log#info "Auth.Failure: %s" cond;
        (match cond with
           | "non-authorized" ->
               print_endline "will register";
           | _ ->
               ()
        )
    | Sasl.AuthError reason ->
        log#crit "Authorization failed: %s" reason;
        Pervasives.exit 127
    | End_of_file ->
        log#info"The connection to the server is lost";
        List.iter (fun proc -> proc ()) global.on_disconnect;
        reconnect account times
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
          
let _ =
  let accounts, plugins = Config.get_config () in
  let () = Plugins.load_plugins plugins in
    if accounts <> [] then
      let account = List.hd accounts in
        reconnect account account.reconnect_times
    else
      Printf.eprintf "no accounts"
