(*
 * (c) 2004-2012 Anastasia Gornostaeva
 *)

open Hooks
open Plugin_command
open Iq

let idle =
  let print_idle env = function
    | None -> "hz"
    | Some el ->
        match XEP_last.decode el with
          | None -> "hz"
          | Some t ->
              Lang.expand_time ~lang:env.env_lang "idle" t.XEP_last.seconds
  in
  let me xmpp env kind jid_from _text =
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_userinfo_idle_me" [])
  in
  let success env text entity el =
    match entity with
      | EntityMe _ ->
          Lang.get_msg env.env_lang "plugin_userinfo_idle_me" []
      | EntityYou _ ->
          Lang.get_msg env.env_lang "plugin_userinfo_idle_you"  
            [print_idle env el]
      | EntityUser _ ->
          Lang.get_msg env.env_lang "plugin_userinfo_idle_somebody" 
            [text; print_idle env el]
      | EntityHost _ ->
          raise BadEntity
  in
    simple_query_entity ~me success ~payload:(XEP_last.make_iq_get ())
      
let uptime =
  let success env text _entity = function
    | None -> "hz"
    | Some el ->
        match XEP_last.decode el with
          | None -> "hz"
          | Some t ->
              let last = Lang.expand_time ~lang:env.env_lang
                "uptime" t.XEP_last.seconds in
                Lang.get_msg env.env_lang "plugin_userinfo_uptime" [text; last]
  in
    simple_query_entity success ~payload:(XEP_last.make_iq_get ())
      
let unknown = "[noname]"

let version =
  let print_version env msgid arg = function
    | Some t ->
      let client =
        if t.XEP_version.name = "" then unknown
        else t.XEP_version.name in
      let version =
        if t.XEP_version.version = "" then unknown
        else t.XEP_version.version in
      let os =
        if t.XEP_version.os = "" then unknown
        else t.XEP_version.os in
        Lang.get_msg env.env_lang msgid (arg @ [client; version; os])
    | _ -> "hz"
  in
  let me xmpp env kind jid_from _text =
    env.env_message xmpp kind jid_from
      (Printf.sprintf "%s %s - %s" Version.name Version.version Iq.os)
  in
  let success env text entity el =
    match entity with
      | EntityMe _ ->
          Printf.sprintf "%s %s - %s" Version.name Version.version Iq.os
      | EntityYou _ ->
          print_version env "plugin_userinfo_version_you" [] el
      | EntityHost _ ->
          print_version env "plugin_userinfo_version_server" [text] el
      | EntityUser _ ->
          print_version env "plugin_userinfo_version_somebody" [text] el
  in
    simple_query_entity2 ~me success XEP_version.get
      
open Netdate
      
let time =
  let print_time env msgid arg = function
    | None -> "hz"
    | Some el ->
        let t = XEP_time.decode el in
        let resp =
          if t.XEP_time.display = "" then
            let netdate = Scanf.sscanf t.XEP_time.utc
              "%4d%2d%2dT%2d:%2d:%d" 
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
          else
            t.XEP_time.display
        in
          Lang.get_msg env.env_lang msgid (arg @ [resp])
  in
  let me xmpp env kind jid_from _text =
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_userinfo_time_me"
         [Strftime.strftime ~tm:(Unix.localtime (Unix.gettimeofday ())) 
            "%H:%M"])
  in
  let success env text entity el =
    match entity with
      | EntityMe _ ->
          Lang.get_msg env.env_lang "plugin_userinfo_time_me"
            [Strftime.strftime ~tm:(Unix.localtime (Unix.gettimeofday ())) 
               "%H:%M"]
      | EntityYou _ ->
          print_time env "plugin_userinfo_time_you" [] el
      | EntityHost _ ->
          print_time env "plugin_userinfo_time_server" [text] el
      | EntityUser _ ->
          print_time env "plugin_userinfo_time_somebody" [text] el
  in
    simple_query_entity ~me success ~payload:(XEP_time.make_iq_get ())
      
let stats =
  let success env text _entity = function
    | None -> "hz"
    | Some el ->
        let stats = XEP_stats.decode el in
        let usersonline =
          try
            let t = List.find (fun t -> t.XEP_stats.name = "users/online") stats
            in t.XEP_stats.value
          with Not_found -> "n/a" in
        let userstotal =
          try
            let t = List.find (fun t -> t.XEP_stats.name = "users/total") stats
            in t.XEP_stats.value
          with Not_found -> "n/a" in
          Printf.sprintf "Stats for %s\nUsers Total: %s\nUsers Online: %s"
            text userstotal usersonline
  in
    simple_query_entity success
      ~payload:(XEP_stats.make_iq_get ["users/online";
                                       "users/total"])
      
let plugin opts =
  add_for_token
    (fun _opts xmpp ->
       add_commands xmpp [("version", version);
                          ("time", time);
                          ("idle", idle);
                          ("uptime", uptime);
                          ("stats", stats)] opts
    )

let _ =
  Plugin.add_plugin "userinfo" plugin
