(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
*)

open Xml
open Xmpp
open Jid
open Types
open Config
open Common

module IqCallback = Map.Make(Id)
let iqcallbacks = ref (IqCallback.empty:
                         (Xmpp.iq_type -> Jid.jid -> Xml.element ->
                            (Xml.element -> unit) -> unit) IqCallback.t)


module XmlnsMap = Map.Make(Id)
let xmlnsmap = ref XmlnsMap.empty

let presencemap = ref []

let on_connect = ref ([]:((Xml.element -> unit) -> unit) list)
let on_disconnect = ref ([]:(unit -> unit) list)
let on_quit = ref ([]:((Xml.element -> unit) -> unit) list)

let commands:
    (string, (string -> Jid.jid -> Xml.element -> local_env ->
                (Xml.element -> unit) -> unit)) Hashtbl.t
    = Hashtbl.create 10

let catchset = ref ([]:(Jid.jid -> Xml.element -> local_env ->
                          (Xml.element -> unit) -> unit) list)
let filters:
    (string, (Jid.jid -> Xml.element -> local_env ->
                 (Xml.element -> unit) -> unit)) Hashtbl.t
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
  | Xmlns of string * (xmpp_event -> jid -> element -> (element -> unit) -> unit)
  | PresenceHandle of (jid -> element -> (element -> unit) -> unit)
        
let register_handle (handler:reg_handle) =
  match handler with
    | Xmlns (xmlns, proc) ->
        xmlnsmap := XmlnsMap.add xmlns proc !xmlnsmap
    | PresenceHandle proc ->
        presencemap := proc :: !presencemap

let process_iq from xml out =
  let id, type_, xmlns = iq_info xml in
    match type_ with
      | `Result
      | `Error ->
          if id <> "" then
            (try
               let f = IqCallback.find id !iqcallbacks in
                 (try f type_ from xml out with exn -> 
                    log#error "[executing iq callback] %s: %s"
                      (Printexc.to_string exn) (element_to_string xml));
                 iqcallbacks := IqCallback.remove id !iqcallbacks
             with Not_found -> 
               ())
      | `Get
      | `Set ->
          (try      
             let f = XmlnsMap.find (get_xmlns xml) !xmlnsmap in
               (try f (Iq (id, type_, xmlns)) from xml out with exn -> 
                  log#error "[executing xmlns callback] %s: %s"
                    (Printexc.to_string exn) (element_to_string xml));
           with Not_found ->
             out (Error.make_error_reply xml `ERR_FEATURE_NOT_IMPLEMENTED)
          )
        
let do_command text from xml env out =
  let word = 
    try String.sub text 0 (String.index text ' ') with Not_found -> text in
  let f = Hashtbl.find commands word in
  let params = try string_after text (String.index text ' ') with _ -> "" in
    try f (trim params) from xml env out with exn -> 
      log#error "[executing command callback] %s: %s"
        (Printexc.to_string exn) (element_to_string xml)
        
let process_message from xml env out =
  let msg_type = safe_get_attr_s xml "type" in
    if msg_type <> "error" then
      let text = 
        try get_cdata xml ~path:["body"] with Not_found -> "" in
        (try do_command text from xml env out with Not_found ->
           List.iter  (fun f -> 
                         try f from xml env out with exn ->
                           log#error "[executing catch callback] %s: %s"
                             (Printexc.to_string exn)
                             (element_to_string xml)
                      ) !catchset)
    else
      ()

let process_presence from xml env out =
    ()
        
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
  let tag = get_tagname xml in
  let env = { env_groupchat = false;
              env_lang = safe_get_attr_s xml "xml:lang";
              env_check_access = default_check_access;
              env_get_entity = default_get_entity } in
    match tag with
      | "message" ->
          process_message from xml env out
      | "presence" ->
          process_presence from xml env out
      | "iq" ->
          process_iq from xml out
      | _ ->
          ()

let rec process_xml next_xml out =
  let xml = next_xml () in
  let from = jid_of_string (get_attr_s xml "from") in
    !dispatch_xml from xml out;
    process_xml next_xml out
            
let _ =
  register_dispatch_xml default_dispatch_xml;
  register_on_disconnect (fun () ->
                            iqcallbacks := IqCallback.empty;
                            xmlnsmap := XmlnsMap.empty;
                            presencemap := [];
                         )

let quit out =
  List.iter (fun proc -> try proc out with exn -> 
               log#error "[quit] %s" (Printexc.to_string exn)
            ) !on_quit;
  Pervasives.exit 0
