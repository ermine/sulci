(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Unix
open Xml
open Xmpp
open Jid
open Error
open Types
open Common
open Hooks
open Iq
        
let idle =
  let print_idle env xml =
    let seconds = get_attr_s xml ~path:["query"] "seconds" in
      Lang.expand_time env.env_lang "idle"  (int_of_string seconds)
  in
  let me =
    fun text from xml env out ->
      make_msg out xml (Lang.get_msg env.env_lang "plugin_userinfo_idle_me" [])
  in
  let success text entity env xml =
    match entity with
      | EntityMe ->
          Lang.get_msg env.env_lang "plugin_userinfo_idle_me" []
      | EntityYou ->
          Lang.get_msg env.env_lang "plugin_userinfo_idle_you" 
            [print_idle env xml]
      | EntityUser ->
          Lang.get_msg env.env_lang "plugin_userinfo_idle_somebody" 
            [text; print_idle env xml]
      | _ ->
          raise BadEntity
  in
    simple_query_entity ~me success "jabber:iq:last"
      
let uptime =
  let success text entity env xml =
    let seconds = get_attr_s xml ~path:["query"] "seconds" in
    let last = Lang.expand_time env.env_lang "uptime" (int_of_string seconds) in
      Lang.get_msg env.env_lang "plugin_userinfo_uptime" [text; last]
  in
    simple_query_entity success "jabber:iq:last"
      
let version =
  let print_version env xml msgid arg =
    let client = try get_cdata xml ~path:["query"; "name"] with 
        Not_found -> "[unknown]" in
    let version = try get_cdata xml ~path:["query"; "version"] with
        Not_found -> "[unknown]" in
    let os = try get_cdata xml ~path:["query"; "os"] with 
        Not_found -> "[unknown]" 
    in
      Lang.get_msg env.env_lang msgid (arg @ [client; version; os])
  in
  let me =
    fun text from xml env out ->
      make_msg out xml 
        (Printf.sprintf "%s %s - %s" Version.name Version.version Jeps.os)
  in
  let success text entity env xml =
    match entity with
      | EntityMe ->
          Printf.sprintf "%s %s - %s" Version.name Version.version Jeps.os
      | EntityYou ->
          print_version env xml "plugin_userinfo_version_you" []
      | EntityHost ->
          print_version env xml "plugin_userinfo_version_server" [text]
      | EntityUser ->
          print_version env xml "plugin_userinfo_version_somebody" [text]
      | _ ->
          raise BadEntity
  in
    simple_query_entity ~me success "jabber:iq:version"
      
open Netdate
      
let time =
  let print_time env xml msgid arg =
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
      Lang.get_msg env.env_lang msgid (arg @ [resp])
  in
  let me =
    fun text from xml env out ->
      make_msg out xml 
        (Lang.get_msg env.env_lang "plugin_userinfo_time_me"
           [Strftime.strftime ~tm:(localtime (gettimeofday ())) 
              "%H:%M"])
  in
  let success text entity env xml =
    match entity with
      | EntityMe ->
          Lang.get_msg env.env_lang "plugin_userinfo_time_me"
            [Strftime.strftime ~tm:(localtime (gettimeofday ())) 
               "%H:%M"]
      | EntityYou ->
          print_time env xml "plugin_userinfo_time_you" []
      | EntityHost ->
          print_time env xml "plugin_userinfo_time_server" [text]
      | EntityUser ->
          print_time env xml "plugin_userinfo_time_somebody" [text]

  in
    simple_query_entity ~me success "jabber:iq:time"
      
let stats =
  let success text entity env xml =
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
  let query_subels = Some [make_element "stat" ["name", "users/online"] [];
                           make_element "stat" ["name", "users/total"] []] in
    simple_query_entity success
      ?query_subels "http://jabber.org/protocol/stats"
      
let _ =
  register_command"version" version;
  register_command"time" time;
  register_command"idle" idle;
  register_command"uptime" uptime;
  register_command"stats" stats
