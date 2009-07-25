(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open XMPP
open XMPP.Network
open Jid
open Types
open Config
open Common

let _ =
  Printexc.record_backtrace true

module IqCallback = Map.Make(Id)
let iqcallbacks = ref (IqCallback.empty:
                         (iq_info -> Jid.jid -> element ->
                            out-> unit) IqCallback.t)


module Ns =
struct
  type t = Xml.namespace
  let compare = compare
end
  
module XmlnsMap = Map.Make(Ns)
let xmlnsmap = ref XmlnsMap.empty

let presencemap = ref []

let on_connect = ref ([]:(out -> unit) list)
let on_disconnect = ref ([]:(unit -> unit) list)
let on_quit = ref ([]:(out -> unit) list)

let commands:
    (string, (string -> Jid.jid -> element -> local_env -> out -> unit))
    Hashtbl.t = Hashtbl.create 10

let catchset = ref ([]:(Jid.jid -> element -> local_env -> out -> unit) list)

let filters: (string, (Jid.jid -> element -> local_env -> out -> unit)) Hashtbl.t
    = Hashtbl.create 5
  
let dispatch_xml = ref (fun _from _xml _out -> ())

let register_on_connect proc =
    on_connect := proc :: !on_connect

let register_on_disconnect proc =
  on_disconnect := proc :: !on_disconnect

let register_on_quit proc =
  on_quit := proc :: !on_quit
    
let register_iq_query_callback id proc =
  iqcallbacks := IqCallback.add id proc !iqcallbacks
    
let register_command (command:string) proc =
  Hashtbl.replace commands command proc

let register_catcher proc =
  catchset := proc :: !catchset

let register_filter name proc =
  Hashtbl.replace name proc filters

let register_dispatch_xml proc = dispatch_xml := proc

type reg_handle =
  | Xmlns of Xml.namespace * (iq_info -> jid -> element -> out -> unit)
  | PresenceHandle of (jid -> element -> out -> unit)
        
let register_handle (handler:reg_handle) =
  match handler with
    | Xmlns (xmlns, proc) ->
        xmlnsmap := XmlnsMap.add xmlns proc !xmlnsmap
    | PresenceHandle proc ->
        presencemap := proc :: !presencemap

let process_iq from xml env out =
  let id = safe_get_attr_value "id" (get_attrs xml) in
    match iq_info xml with
      | IqGet el
      | IqSet el as iq -> (
          try
            let xmlns = get_namespace (get_qname el) in
            let f = XmlnsMap.find xmlns !xmlnsmap in
              try f iq from xml out with exn ->
                log#error "[executing xmlns callback] %s"
                  (Printexc.to_string exn);
                log#debug "%s" (Printexc.get_backtrace ())
          with Not_found ->
            out (make_error_reply StanzaError.ERR_FEATURE_NOT_IMPLEMENTED xml)
        )        
      | IqResult el as iq ->
          if id <> "" then (
            try
              let f = IqCallback.find id !iqcallbacks in
                try f iq from xml out with exn ->
                  log#error "[executing iq callback] %s"
                    (Printexc.to_string exn);
                  log#debug "%s" (Printexc.get_backtrace ());
                  iqcallbacks := IqCallback.remove id !iqcallbacks;
            with Not_found -> ()
          )
      | IqError err as iq ->
          if id <> "" then (
            try
              let f = IqCallback.find id !iqcallbacks in
                try f iq from xml out with exn ->
                  log#error "[executing iq callback] %s"
                    (Printexc.to_string exn);
                  log#debug "%s" (Printexc.get_backtrace ());                  
                  iqcallbacks := IqCallback.remove id !iqcallbacks;
            with Not_found -> ()
          )

let do_command text from xml env out =
  let word = 
    try String.sub text 0 (String.index text ' ') with Not_found -> text in
  let f = Hashtbl.find commands word in
  let params = try string_after text (String.index text ' ') with _ -> "" in
    try f (trim params) from xml env out with exn -> 
      log#error "[executing command callback] %s" (Printexc.to_string exn);
      log#debug "%s" (Printexc.get_backtrace ())
        
let process_message from xml env out =
  let msg_type = safe_get_attr_value "type" (get_attrs xml) in
    if msg_type <> "error" then
      let text = 
        try get_cdata (get_subelement (ns_client, "body") xml)
        with Not_found -> ""
      in
        try do_command text from xml env out
        with Not_found ->
          List.iter (fun f ->
                       try f from xml env out
                       with exn ->
                         log#error "[executing catch callback] %s"
                           (Printexc.to_string exn);
                         log#debug "%s" (Printexc.get_backtrace ())
                    ) !catchset
            
let process_presence _from _xml _env _out = ()
        
let default_check_access (jid:jid) classname =
  List.exists (fun (jid', name) ->
                 if name = classname &&
                   jid'.lnode = jid.lnode && 
                   jid'.ldomain = jid.ldomain then
                     true
                 else
                   false
              ) Config.acls

let default_get_entity text from =
  if text = "" then
    EntityYou, from
  else
    try
      let jid = jid_of_string text in
        if jid.lnode = "" then (
          dnsprep jid.ldomain;
          EntityHost, jid
        )
        else if from.lnode = jid.lnode && from.ldomain = jid.ldomain then
          EntityYou, jid
        else
          EntityUser, jid
    with _ ->
      raise BadEntity
    
let default_dispatch_xml from xml out =
  let tag = get_name (get_qname xml) in
  let env = { env_groupchat = false;
              env_lang = safe_get_attr_value ~ns:ns_xml "lang" (get_attrs xml);
              env_check_access = default_check_access;
              env_get_entity = default_get_entity } in
    match tag with
      | "message" ->
          process_message from xml env out
      | "presence" ->
          process_presence from xml env out
      | "iq" ->
          process_iq from xml env out
      | _ ->
          ()

let rec process_xml myjid p inch ouch =
  next_xml inch p () >>=
    (function
       | Xmlstream.Stanza el ->
           let from = jid_of_string (get_from el) in
             !dispatch_xml from el
               (fun xml ->
                  ignore (send ouch (Xmlstream.stanza_serialize p xml)));
             process_xml myjid p inch ouch
       | _ ->
           fail (Error "unknown stanza")
    )
            
let _ =
  register_dispatch_xml default_dispatch_xml;
  register_on_disconnect (fun () ->
                            iqcallbacks := IqCallback.empty;
                            xmlnsmap := XmlnsMap.empty;
                            presencemap := []
                         )

let quit out =
  List.iter (fun proc ->
               try proc out with exn ->
                 log#error "[quit] %s" (Printexc.to_string exn);
                 log#debug "%s" (Printexc.get_backtrace ())
            ) !on_quit;
  Pervasives.exit 0
