(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open Xmpp
open Jid
open Types
open Common
open Hooks
open Iq

let ping text from xml env out =
  let success starttime text entity env xml =
    let diff = Lang.float_seconds env.env_lang "ping" 
      (Unix.gettimeofday () -. starttime) in
      match entity with
        | EntityMe ->
            Lang.get_msg env.env_lang "plugin_ping_pong_from_me" [diff]
        | EntityYou ->
            Lang.get_msg env.env_lang "plugin_ping_pong_from_you" [diff]
        | _ ->
            Lang.get_msg env.env_lang "plugin_ping_pong_from_somebody"
              [text; diff]
  in
  let starttime = Unix.gettimeofday () in
    simple_query_entity ~error_exceptions:[`ERR_FEATURE_NOT_IMPLEMENTED]
      (success starttime) "jabber:iq:version" text from xml env out

let _ =
  register_command "ping" ping
    
