(*
 * (c) 2004-2012 Anastasia Gornostaeva
 *)

open Hooks
open Plugin_command
open Iq

module Last = XEP_last.Make(XMPPClient)
module Time = XEP_time.Make(XMPPClient)
module Ver = XEP_version.Make(XMPPClient)
module Stats = XEP_stats.Make(XMPPClient)

let idle =
  let print_idle env = function
    | None -> "hz"
    | Some el ->
        match Last.decode el with
          | None -> "hz"
          | Some t ->
              Lang.expand_time ~lang:env.env_lang "idle" t.Last.seconds
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
    simple_query_entity ~me success ~payload:(Last.make_iq_get ())
      
let uptime =
  let success env text _entity = function
    | None -> "hz"
    | Some el ->
        match Last.decode el with
          | None -> "hz"
          | Some t ->
              let last = Lang.expand_time ~lang:env.env_lang
                "uptime" t.Last.seconds in
                Lang.get_msg env.env_lang "plugin_userinfo_uptime" [text; last]
  in
    simple_query_entity success ~payload:(Last.make_iq_get ())
      
let unknown = "[noname]"

let version =
  let print_version env msgid arg = function
    | Some t ->
      let client =
        if t.Ver.name = "" then unknown
        else t.Ver.name in
      let version =
        if t.Ver.version = "" then unknown
        else t.Ver.version in
      let os =
        if t.Ver.os = "" then unknown
        else t.Ver.os in
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
    simple_query_entity2 ~me success Ver.get
      
open Netdate
      
let time =
  let print_time env msgid arg = function
    | None -> "hz"
    | Some el ->
        let t = Time.decode el in
        let resp =
          if t.Time.display = "" then
            let netdate = Scanf.sscanf t.Time.utc
              "%4d%2d%2dT%2d:%2d:%d" 
              (fun year month day hour min sec -> 
                 { year = year;
                   month = month;
                   day = day;
                   hour = hour;
                   minute = min;
                   second = sec;
                   zone = 0;
                   week_day = 0;
                   nanos = 0
                 }) in
            let f = Netdate.since_epoch netdate in
              Netdate.mk_mail_date f
          else
            t.Time.display
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
    simple_query_entity ~me success ~payload:(Time.make_iq_get ())
      
let stats =
  let success env text _entity = function
    | None -> "hz"
    | Some el ->
        let stats = Stats.decode el in
        let usersonline =
          try
            let t = List.find (fun t -> t.Stats.name = "users/online") stats
            in t.Stats.value
          with Not_found -> "n/a" in
        let userstotal =
          try
            let t = List.find (fun t -> t.Stats.name = "users/total") stats
            in t.Stats.value
          with Not_found -> "n/a" in
          Printf.sprintf "Stats for %s\nUsers Total: %s\nUsers Online: %s"
            text userstotal usersonline
  in
    simple_query_entity success
      ~payload:(Stats.make_iq_get ["users/online";
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
