(*
 * (c) 2005-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
*)

open Unix
open Xml

let proc ev event from (xml:Xml.element) (out:Xml.element -> unit) =
  Event.sync (Event.send ev xml)

let sulci_rpc out in_chan out_chan () =
  let xml = input_value in_chan in
    match get_tagname xml with
      | "iq" ->
          let id = get_attr_s xml "id" in
          let ev = Event.new_channel () in
            Hooks.register_handle (Hooks.Id (id, proc ev));
            out xml;
            let reply = Event.sync (Event.receive ev) in
              output_value out_chan reply;
              flush out_chan;
              close_out out_chan
      | _ -> (* todo: figure our how to send *)
          close_out out_chan
            
let _ =
  let port = 5221 in
  let inet_addr = inet_addr_any in
  let sockaddr = ADDR_INET (inet_addr, port) in
  let fd = socket PF_INET SOCK_STREAM 0 in
  let () = setsockopt fd SO_REUSEADDR true in
    
  let server out =
    try
      Unix.bind fd sockaddr;
      Unix.listen fd 10;
      while true do
        let client, _ = Unix.accept fd in
        let in_chan = Unix.in_channel_of_descr client
        and out_chan = Unix.out_channel_of_descr client in
          ignore (Thread.create (sulci_rpc out in_chan out_chan) ())
      done
    with Unix.Unix_error (code, syscall, _) ->
      Logger.out
        (Printf.sprintf "problem with %s: %s\n" syscall
           (Unix.error_message code));
      Thread.exit ()
  in
  let start out =
    ignore (Thread.create server out)
  in
    Hooks.register_handle (Hooks.OnStart start)
