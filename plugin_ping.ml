(*
 * (c) 2004-2012 Anastasia Gornostaeva
 *)

open StanzaError
open JID
open XEP_version
open Hooks
open Plugin_command
open XMPPClient

module V = XEP_version.Make(XMPPClient)

let ping xmpp env kind jid_from text =
  let success starttime env text entity _result =
    let diff = Lang.float_seconds env.env_lang "ping" 
      (Unix.gettimeofday () -. starttime) in
      match entity with
        | EntityMe _ ->
          Lang.get_msg env.env_lang "plugin_ping_pong_from_me" [diff]
        | EntityYou _ ->
          Lang.get_msg env.env_lang "plugin_ping_pong_from_you" [diff]
        | EntityHost _
        | EntityUser _ ->
          Lang.get_msg env.env_lang "plugin_ping_pong_from_somebody"
            [text; diff]
  in
    Iq.simple_query_entity2 ~error_exceptions:[ERR_FEATURE_NOT_IMPLEMENTED]
      (success (Unix.gettimeofday ())) V.get
      xmpp env kind jid_from text 

    
let plugin opts =
  add_for_token
    (fun _opts xmpp ->
       add_commands xmpp [("ping", ping)] opts
    )
    
let _ =
  Plugin.add_plugin "ping" plugin
