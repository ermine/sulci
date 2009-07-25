(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Unix
open Xml
open XMPP
open Jid
open Types
open Common
open Hooks
open Iq

let ns_last = Some "jabber:iq:last"
let ns_version = Some "jabber:iq:version"
let ns_stats = Some "http://jabber.org/protocol/stats"
let ns_time = Some "jabber:iq:time"
  
let idle =
  let print_idle env = function
    | None -> "hz"
    | Some el ->
        let seconds = get_attr_value "seconds" (get_attrs el) in
          Lang.expand_time ~lang:env.env_lang "idle"  (int_of_string seconds)
  in
  let me =
    fun _text _from xml env out ->
      make_msg out xml (Lang.get_msg env.env_lang "plugin_userinfo_idle_me" [])
  in
  let success text entity env el =
    match entity with
      | EntityMe ->
          Lang.get_msg env.env_lang "plugin_userinfo_idle_me" []
      | EntityYou ->
          Lang.get_msg env.env_lang "plugin_userinfo_idle_you"  
            [print_idle env el]
      | EntityUser ->
          Lang.get_msg env.env_lang "plugin_userinfo_idle_somebody" 
            [text; print_idle env el]
      | EntityHost ->
          raise BadEntity
  in
    simple_query_entity ~me success
      ~payload:(make_element (ns_last, "query") [] [])
      
let uptime =
  let success text _entity env = function
    | None -> "hz"
    | Some el ->
        let seconds = get_attr_value "seconds" (get_attrs el) in
        let last = Lang.expand_time ~lang:env.env_lang
          "uptime" (int_of_string seconds) in
          Lang.get_msg env.env_lang "plugin_userinfo_uptime" [text; last]
  in
    simple_query_entity success
      ~payload:(make_element (ns_last, "query") [] [])
      
let version =
  let print_version env msgid arg = function
    | None -> "hz"
    | Some el ->
        let client =
          try get_cdata (get_subelement (ns_version, "name") el)
          with Not_found -> "[unknown]" in
        let version =
          try get_cdata (get_subelement (ns_version, "version)") el)
          with Not_found -> "[unknown]" in
        let os =
          try get_cdata (get_subelement (ns_version, "os") el)
          with Not_found -> "[unknown]" 
        in
          Lang.get_msg env.env_lang msgid (arg @ [client; version; os])
  in
  let me =
    fun _text _from xml _env out ->
      make_msg out xml 
        (Printf.sprintf "%s %s - %s" Version.name Version.version Jeps.os)
  in
  let success text entity env el =
    match entity with
      | EntityMe ->
          Printf.sprintf "%s %s - %s" Version.name Version.version Jeps.os
      | EntityYou ->
          print_version env "plugin_userinfo_version_you" [] el
      | EntityHost ->
          print_version env "plugin_userinfo_version_server" [text] el
      | EntityUser ->
          print_version env "plugin_userinfo_version_somebody" [text] el
  in
    simple_query_entity ~me success
      ~payload:(make_element (ns_version, "query") [] [])
      
open Netdate
      
let time =
  let print_time env msgid arg = function
    | None -> "hz"
    | Some el ->
        let resp =
          try get_cdata (get_subelement (ns_time, "display") el)
          with Not_found ->
            let utc = get_cdata (get_subelement (ns_time, "utc") el) in
            let netdate = Scanf.sscanf utc "%4d%2d%2dT%2d:%2d:%d" 
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
    fun _text _from xml env out ->
      make_msg out xml 
        (Lang.get_msg env.env_lang "plugin_userinfo_time_me"
           [Strftime.strftime ~tm:(localtime (gettimeofday ())) 
              "%H:%M"])
  in
  let success text entity env el =
    match entity with
      | EntityMe ->
          Lang.get_msg env.env_lang "plugin_userinfo_time_me"
            [Strftime.strftime ~tm:(localtime (gettimeofday ())) 
               "%H:%M"]
      | EntityYou ->
          print_time env "plugin_userinfo_time_you" [] el
      | EntityHost ->
          print_time env "plugin_userinfo_time_server" [text] el
      | EntityUser ->
          print_time env "plugin_userinfo_time_somebody" [text] el
  in
    simple_query_entity ~me success
      ~payload:(make_element (ns_time, "query") [] [])
      
let stats =
  let success text _entity _env = function
    | None -> "hz"
    | Some el ->
        let stats_data = get_subelements (ns_stats, "stat") el in
        let data = List.map (fun z ->
                               get_attr_value "name" (get_attrs z),
                               try get_attr_value "value" (get_attrs z)
                               with Not_found -> "unknown" ) stats_data in
          Printf.sprintf "Stats for %s\nUsers Total: %s\nUsers Online: %s"
            text
            (List.assoc "users/total" data)
            (List.assoc "users/online" data)
  in
    simple_query_entity success
      ~payload:(make_element (ns_stats, "query") []
                  [make_element (ns_stats, "stat")
                     [make_attr "name" "users/online"] [];
                   make_element (ns_stats, "stat")
                     [make_attr "name" "users/total"] []])
      
let _ =
  register_command "version" version;
  register_command "time" time;
  register_command "idle" idle;
  register_command "uptime" uptime;
  register_command "stats" stats
