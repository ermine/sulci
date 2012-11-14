(*
 * (c) 2004-2012 Anastasia Gornostaeva
 *)

open StreamError
open JID
open Hooks
open Config
  
let _ = Printexc.record_backtrace true

module SimpleTransport =
struct
  type 'a z = 'a UnitMonad.t
  type fd = unit
      
  type socket = {
    inc : in_channel;
    outc : out_channel;
  }

  let can_tls = false
  let can_compress = false

  let open_connection sockaddr =
    let inc, outc = Unix.open_connection sockaddr in
      {inc;
       outc
      }
  let read s buf start len =
    let size = input s.inc buf start len in
      size

  let write s str =
    output_string s.outc str;
    flush s.outc

  let close s = close_in s.inc

end

module LogTraffic  (T : XMPPClient.Socket)
  (L : sig val logfile : out_channel end) =
struct
  open UnitMonad

  type t = T.t
  let socket = T.socket

  let read s buf start len =
    let size = T.read s buf start len in
      if size = 0 then (
        output_string L.logfile "IN CLOSED\n";
        flush L.logfile;
        size
      ) else (
        output_string L.logfile "IN: ";
        output_string L.logfile (String.sub buf start size);
        output_string L.logfile "\n";
        flush L.logfile;
        size
      )

  let write s str =
    output_string L.logfile "OUT: ";
    output_string L.logfile str;
    output_string L.logfile "\n";
    flush L.logfile;
    T.write s str

  let close = T.close
end

open XMPPClient

let session xmpp =
  log#info "Connected to %s!" xmpp.myjid.domain;

  XMPPClient.register_stanza_handler xmpp (ns_client, "message")
    (XMPPClient.parse_message ~callback:message_callback
       ~callback_error:message_error);
  XMPPClient.register_stanza_handler xmpp (ns_client, "presence")
    (XMPPClient.parse_presence ~callback:presence_callback
       ~callback_error:presence_error);
  
  Iq.features xmpp;
    
  (* workaround for wildfire *)
  send_presence xmpp ();

  List.iter (fun proc -> try proc xmpp with exn ->
    log#error "sulci.ml: %s" (Printexc.to_string exn);
    log#debug "%s" (Printexc.get_backtrace ())
  ) (List.rev xmpp.user_data.on_connect)

let run account =
  let myjid =
    if account.resource = "" then
      account.jid
    else
      replace_resource account.jid account.resource
  in
  let () = log#info "Creating a token for %s" (string_of_jid myjid) in

  let host, port =
    (if account.ip = "" then account.jid.domain else account.ip),
    (match account.port with
       | None -> 5222
       | Some i -> i
    )
  in

  let inet_addr =
    try Unix.inet_addr_of_string host
    with Failure("inet_addr_of_string") ->
      (Unix.gethostbyname host).Unix.h_addr_list.(0) in
  let sockaddr = Unix.ADDR_INET (inet_addr, port) in
  let user_data = {
    Hooks.deflang = !Lang.deflang;
    Hooks.max_stanza_length = account.Config.max_stanza_length;
    Hooks.max_message_length = account.Config.max_message_length;
    on_connect = [];
    on_disconnect = [];
    presence_hooks = [];
    message_hooks = [];
    skey = "abc"
  } in
    Hooks.run_for_token [] user_data;
    let rec reconnect times =
      if times >= 0 then
        let socket_data = SimpleTransport.open_connection sockaddr in
        let module Socket_module =
            struct
              type t = SimpleTransport.socket
              let socket = socket_data
              include SimpleTransport
            end in
        let socket_module =
          if account.rawxml_log = "" then
            (module Socket_module : XMPPClient.Socket)
          else
            let module Socket_module =
                struct
                  include LogTraffic(Socket_module)
                    (struct let logfile = open_out account.rawxml_log end)
                end in
              (module Socket_module : XMPPClient.Socket)
        in
          try
            XMPPClient.setup_session
              ~user_data
              ~myjid
              ~plain_socket:socket_module
              ~password:account.password session >>= fun session_data ->
            XMPPClient.parse session_data >>=
              (fun () ->
                let module S = (val session_data.socket : Socket) in
                  S.close S.socket
              )
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
                ()
              | _ ->
                ()
            )
          | Sasl.Error reason ->
            log#crit "Authorization failed: %s" reason;
            Pervasives.exit 127
          | End_of_file ->
            log#info"The connection to the server is lost";
            List.iter (fun proc -> proc ()) (List.rev user_data.on_disconnect);
            reconnect times
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
    reconnect account.reconnect_times
      
let rec launch r =
  let pid = Unix.fork () in
    if pid = 0 then
      r ()
    else
      Printf.printf "Process %d detached" pid
        
let main accounts plugins () =
  let () = Plugin.load_plugins plugins in
  let account = List.hd accounts in
    run account
      
let () =
  let daemon, ((langdir, deflang), accounts, plugins, logging) =
    Config.get_config () in
  let () =
    match logging with
      | None -> ()
      | Some (level, dst) ->
        Hooks.log#set_max_level level;
        Hooks.log#set_destination dst in
    Lang.langdir := langdir;
    Lang.deflang := deflang;
    if accounts <> [] then
      if daemon then (
        ignore (Unix.setsid ());
        launch (main accounts plugins)
      )
      else
        main accounts plugins ()
    else
      Printf.eprintf "no account available"
        
