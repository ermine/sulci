(*
 * (c) 2004-2008 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
*)

open Xml
open Xmpp
open Jid
open Jid
open Types
open Config
open Common
open Nicks

module IdMap = Map.Make(Id)
let idmap = ref IdMap.empty

module XmlnsMap = Map.Make(Id)
let xmlnsmap = ref XmlnsMap.empty

let presencemap = ref []

module CommandMap = Map.Make(Id)
let commands = ref CommandMap.empty

let onstart =
  let on_start out =
    GroupchatMap.iter (fun (lnode, ldomain) env ->
                         out (Muc.join_room env.mynick (lnode, ldomain)))
      !groupchats
  in
    ref [on_start]
      
let onquit = ref []
let catchset = ref []
let filters = ref []

type reg_handle =
  | Xmlns of 
      string * (xmpp_event -> jid -> element -> (element -> unit) -> unit)
  | Id of string * (xmpp_event -> jid -> element -> (element -> unit) -> unit)
  | Command of string * 
      (string -> xmpp_event -> jid -> element -> string -> (element -> unit) ->
         unit)
  | OnStart of ((element -> unit) -> unit)
  | OnQuit of ((element -> unit) -> unit)
  | Catch of (xmpp_event -> jid -> element -> string -> (element -> unit) ->
                unit)
  | Filter of (xmpp_event -> jid -> element -> (element -> unit) -> unit)
  | PresenceHandle of 
      (xmpp_event -> jid -> element -> (element -> unit) -> unit)
        
let register_handle (handler:reg_handle) =
  match handler with
    | Xmlns (xmlns, proc) ->
        xmlnsmap := XmlnsMap.add xmlns proc !xmlnsmap
    | Id (id, proc) ->
        idmap := IdMap.add id proc !idmap
    | Command (command, proc) ->
        commands := CommandMap.add command proc !commands
    | OnStart proc ->
        onstart := proc :: !onstart;
    | OnQuit proc ->
        onquit := proc :: !onquit;
    | Catch proc ->
        catchset := proc :: !catchset
    | Filter proc ->
        filters := proc :: !filters
    | PresenceHandle proc ->
        presencemap := proc :: !presencemap

let process_iq event from xml out =
  match event with
    | Iq (id, type_, xmlns) ->
        (match type_ with
           | `Result
           | `Error ->
               if id <> "" then
                 (try
                    let f = IdMap.find id !idmap in
                      (try f event from xml out with exn -> 
                         log#error "[executing iq callback] %s: %s"
                           (Printexc.to_string exn) (element_to_string xml));
                      idmap := IdMap.remove id !idmap
                  with Not_found -> 
                    ())
           | `Get
           | `Set ->
               (try      
                  let f = XmlnsMap.find (get_xmlns xml) !xmlnsmap in
                    (try f event from xml out with exn -> 
                       log#error "[executing xmlns callback] %s: %s"
                         (Printexc.to_string exn) (element_to_string xml));
                with Not_found -> ()))
    | _ -> ()
        
let do_command text event from xml lang out =
  let word = 
    try String.sub text 0 (String.index text ' ') with Not_found -> text in
  let f = CommandMap.find word !commands in
  let params = try string_after text (String.index text ' ') with _ -> "" in
    try f (trim params) event from xml lang out with exn -> 
      log#error "[executing command callback] %s: %s"
        (Printexc.to_string exn) (element_to_string xml)
        
let process_message event from xml out =
  let lang = Muc.get_lang from xml in
    match event with
      | MUC_message (msg_type, nick, text) ->
          if msg_type <> `Error then
            if text <> "" then
              try
                let room_env = GroupchatMap.find (from.lnode, from.ldomain)
                  !groupchats in
                  match msg_type with
                    | `Groupchat ->
                        if from.lresource <> room_env.mynick && 
                          nick = "" then
                            do_command text event from xml lang out
                        else
                          raise Not_found
                    | _ ->
                        do_command text event from xml lang out
              with Not_found ->
                List.iter  (fun f -> 
                              try f event from xml lang out with exn ->
                                log#error "[executing catch callback] %s: %s"
                                  (Printexc.to_string exn)
                                  (element_to_string xml)
                           ) !catchset 
            else
              List.iter  (fun f -> 
                            try f event from xml lang out with exn ->
                              log#error "[executing catch callback] %s: %s"
                                (Printexc.to_string exn)
                                (element_to_string xml)
                         ) !catchset 
      | Message ->
          if safe_get_attr_s xml "type" <> "error" then
            let text = 
              try get_cdata xml ~path:["body"] with Not_found -> "" in
              (try do_command text event from xml lang out with Not_found ->
                 List.iter  (fun f -> 
                               try f event from xml lang out with exn ->
                                 log#error "[executing catch callback] %s: %s"
                                   (Printexc.to_string exn)
                                   (element_to_string xml)
                            ) !catchset)
      | _ -> ()
        
exception Filtered
  
let rec process_xml next_xml out =
  let xml = next_xml () in
  let from = jid_of_string (get_attr_s xml "from") in
  let room = (from.lnode, from.ldomain) in
  let tag = get_tagname xml in
  let get_event () =
    match tag with
      | "presence" ->
          if GroupchatMap.mem room !groupchats then
            Muc.process_presence from xml out
          else
            Presence
      | "message" ->
          if GroupchatMap.mem room !groupchats then
            Muc.process_message from xml out
          else
            Message
      | "iq" ->
          let id, type_, xmlns = iq_info xml in
            Iq (id, type_, xmlns)
      | _ ->
          raise InvalidStanza
  in
    (try
       let event = get_event () in
         Muc_log.process_log event from xml;
         List.iter (fun proc -> proc event from xml out) !filters;
         (match event with
            | Iq _ ->
                process_iq event from xml out
            | MUC_message _
            | Message ->
              process_message event from xml out;
            | _ ->
                let lang = Muc.get_lang from xml in
                  List.iter (fun proc -> 
                               try proc event from xml lang out with exn ->
                                 log#error "[executing catch callback] %s: %s"
                                   (Printexc.to_string exn)
                                   (element_to_string xml)
                            ) !catchset
         );
     with
       | InvalidStanza as exn ->
           log#error "[Invalid stanza] %s: %s"
             (Printexc.to_string exn)
             (element_to_string xml)
       | Filtered -> ()
       | exn ->
           log#error "[process_xml] %s: %s"
             (Printexc.to_string exn)
             (element_to_string xml)
    );
    process_xml next_xml out
      
let quit out =
  List.iter (fun proc -> try proc out with exn -> 
               log#error "[quit] %s" (Printexc.to_string exn)
            ) !onquit;
  Pervasives.exit 0
    
let check_access (jid:jid) classname =
  let find_acl who =
    let acls = get_subels Config.config ~tag:"acl" in
      if List.exists (fun a -> 
                        let jid = jid_of_string (get_attr_s a "jid") in
                          if jid.lnode = who.lnode && 
                            jid.ldomain = who.ldomain &&
                            get_attr_s a "class" = classname then
                              true else false) acls 
      then true else false
  in
    try 
      let env = GroupchatMap.find (jid.lnode, jid.ldomain) !groupchats in
      let nick = jid.lresource in
      let item = Nicks.find nick env.nicks in
        match item.jid with
          | None -> false (* TODO? *)
          | Some j -> find_acl j
    with Not_found ->
      find_acl jid
        
        
type entity = [
| `User of jid
| `Nick of string
| `Mynick of string
| `You
| `Host of jid
]

exception BadEntity

let get_entity text event from =
  match event with
    | Message ->
        if text = "" then
          `You
        else (
          try
            let jid = jid_of_string text in
              if jid.lnode = "" then (
                dnsprep jid.ldomain;
                `Host jid
              )
              else if from.string = jid.string then
                `You
              else
                `User jid
          with _ ->
            raise BadEntity
        )
    | MUC_message _ ->
        if text = "" then
          `You
        else
          let room = from.lnode, from.ldomain in
          let room_env = GroupchatMap.find room !groupchats in
            if room_env.mynick = text then
              `Mynick text
            else if Nicks.mem text room_env.nicks then
              if from.resource = text then
                `You
              else
                `Nick text
            else (
              try
                let jid = jid_of_string text in
                  if jid.lnode = "" then (
                    dnsprep jid.ldomain;
                    `Host jid
                  )
                  else
                    `User jid
              with _ ->
                raise BadEntity
            )
    | _ ->
        raise BadEntity
          
