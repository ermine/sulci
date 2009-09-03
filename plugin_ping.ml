(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open StanzaError
open XMPP
open Jid
open Xep_version
open Hooks
open Plugin_command

let ping =
  let success starttime env text entity _el =
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
    fun xmpp env kind jid_from text ->
      Iq.simple_query_entity ~error_exceptions:[ERR_FEATURE_NOT_IMPLEMENTED]
        (success (Unix.gettimeofday ())) ~payload:(Xep_version.make_iq_get ())
        xmpp env kind jid_from text

    
let plugin opts =
  add_commands [("ping", ping)] opts
    
let _ =
  add_plugin "ping" plugin
