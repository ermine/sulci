(*
 * (c) 2004-2008 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open Xmpp
open Jid
open Common
open Unix
open Hooks
open Types
open Nicks
open Muc
open Error
open Iq

let status text event from xml out =
  match event with
    | MUC_message (msg_type, _, _) ->
        let entity = if text = "" then from.lresource else
          Stringprep.resourceprep text in
          (try
             let item = Nicks.find entity
               (GroupchatMap.find (from.lnode, from.ldomain) 
                  !groupchats).nicks in
               make_msg out xml ((if item.status = "" then ""
                                  else item.status ^ " ") ^
                                   "[" ^ (match item.show with
                                            | `Online -> "online"
                                            | `Away -> "away"
                                            | `DND -> "dnd"
                                            | `Chat -> "free for chat"
                                            | `XA -> "xa") ^ "]")
           with _ ->
             make_msg out xml 
               (Lang.get_msg "plugin_userinfo_status_whose" []))
    | _ -> ()
        
let idle =
  let print_idle lang xml =
    let seconds = get_attr_s xml ~path:["query"] "seconds" in
      Lang.expand_time ~lang "idle"  (int_of_string seconds)
  in
  let me =
    fun text event from xml out ->
      make_msg out xml (Lang.get_msg ~xml "plugin_userinfo_idle_me" [])
  in
  let entity_to_jid entity event from =
    match entity with
      | `Mynick nick
      | `Nick nick ->
          string_of_jid {from with resource = nick; lresource = nick}
      | `You ->
          string_of_jid from
      | `User user ->
          user.string
      | `Host _ ->
          raise BadEntity
  in
  let success text entity lang xml =
    match entity with
      | `Mynick mynick ->
          Lang.get_msg ~lang "plugin_userinfo_idle_me" []
      | `You ->
          Lang.get_msg ~lang "plugin_userinfo_idle_you" 
            [print_idle lang xml]
      | `Nick _
      | `User _ ->
          Lang.get_msg ~lang "plugin_userinfo_idle_somebody" 
            [text; print_idle lang xml]
      | _ ->
          raise BadEntity
  in
    simple_query_entity ~me ~entity_to_jid success "jabber:iq:last"
      
let uptime =
  let entity_to_jid entity event from =
    match entity with
      | `Host host ->
          if host.lresource <> "" then
            raise BadEntity
          else
            host.domain
      | _ -> raise BadEntity
  in
  let success text entity lang xml =
    let seconds = get_attr_s xml ~path:["query"] "seconds" in
    let last = Lang.expand_time ~lang "uptime" (int_of_string seconds) in
      Lang.get_msg ~lang "plugin_userinfo_uptime" [text; last]
  in
    simple_query_entity ~entity_to_jid success "jabber:iq:last"
      
let version =
  let print_version lang xml msgid arg =
    let client = try get_cdata xml ~path:["query"; "name"] with 
        Not_found -> "[unknown]" in
    let version = try get_cdata xml ~path:["query"; "version"] with
        Not_found -> "[unknown]" in
    let os = try get_cdata xml ~path:["query"; "os"] with 
        Not_found -> "[unknown]" 
    in
      Lang.get_msg ~lang msgid (arg @ [client; version; os])
  in
  let me =
    fun text event from xml out ->
      make_msg out xml 
        (Printf.sprintf "%s %s - %s" Version.name Version.version Jeps.os)
  in
  let success text entity lang xml =
    match entity with
      | `Mynick mynick ->
          Printf.sprintf "%s %s - %s" Version.name Version.version Jeps.os
      | `You ->
          print_version lang xml "plugin_userinfo_version_you" []
      | `Nick nick ->
          print_version lang xml "plugin_userinfo_version_somebody" [text]
      | `Host host ->
          print_version lang xml "plugin_userinfo_version_server" [text]
      | `User user ->
          print_version lang xml "plugin_userinfo_version_somebody" [text]
  in
    simple_query_entity ~me success "jabber:iq:version"
      
open Netdate
      
let time =
  let print_time lang xml msgid arg =
    let resp =
      try
        get_cdata xml ~path:["query"; "display"]
      with Not_found ->
        let utc = get_cdata xml ~path:["query"; "utc"] in
        let netdate =Scanf.sscanf utc "%4d%2d%2dT%2d:%2d:%d" 
          (fun year month day hour min sec -> 
             { year = year;
               month = month;
               day = day;
               hour = hour;
               minute = min;
               second = sec;
               zone = 0;
               week_day = 0
             }) in
        let f = Netdate.since_epoch netdate in
          Netdate.mk_mail_date f
    in         
      Lang.get_msg ~lang msgid (arg @ [resp])
  in
  let me =
    fun text event from xml out ->
      make_msg out xml 
        (Lang.get_msg ~xml "plugin_userinfo_time_me"
           [Strftime.strftime ~tm:(localtime (gettimeofday ())) 
              "%H:%M"])
  in
  let success text entity lang xml =
    match entity with
      | `Mynick mynick ->
          Lang.get_msg ~lang "plugin_userinfo_time_me"
            [Strftime.strftime ~tm:(localtime (gettimeofday ())) 
               "%H:%M"]
      | `You ->
          print_time lang xml "plugin_userinfo_time_you" []
      | `Nick nick ->
          print_time lang xml "plugin_userinfo_time_somebody" [text]
      | `Host host ->
          print_time lang xml "plugin_userinfo_time_server" [text]
      | `User user ->
          print_time lang xml "plugin_userinfo_time_somebody" [text]
  in
    simple_query_entity ~me success "jabber:iq:time"
      
let stats =
  let entity_to_jid entity event from =
    match entity with
      | `Host host ->
          if host.lresource = "" then
            host.domain
          else
            raise BadEntity
      | _ ->
          raise BadEntity
  in
  let success text entity lang xml =
    let stats_data = get_subels xml ~path:["query"] ~tag:"stat" in
    let data = List.map (fun z -> get_attr_s z "name",     
                           try 
                             get_attr_s z "value"
                           with Not_found -> "unknown" ) stats_data in
      Printf.sprintf "Stats for %s\nUsers Total: %s\nUsers Online: %s"
        text
        (List.assoc "users/total" data)
        (List.assoc "users/online" data)
  in
  let query_subels = Some [Xmlelement ("stat", ["name", "users/online"], []);
                           Xmlelement ("stat", ["name", "users/total"], [])
                          ] in
    simple_query_entity ~entity_to_jid success
      ?query_subels "http://jabber.org/protocol/stats"
      
let _ =
  Hooks.register_handle (Command ("version", version));
  Hooks.register_handle (Command ("time", time));
  Hooks.register_handle (Command ("idle", idle));
  Hooks.register_handle (Command ("uptime", uptime));
  Hooks.register_handle (Command ("stats", stats));
  Hooks.register_handle (Command ("status", status));
