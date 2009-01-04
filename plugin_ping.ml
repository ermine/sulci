(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open Xmpp
open Jid
open Types
open Common
open Hooks

let ping text from xml env out =
  try
    let entity = get_entity text from env in
    let to_ =
      match entity with
        | `Mynick nick
        | `Nick nick ->
            string_of_jid {from with resource = nick; lresource = nick}
        | `You ->
            string_of_jid from
        | `User user ->
            if user.lresource = "" then
              raise BadEntity
            else
              user.string
        | `Host host ->
            host.domain
        | _ ->
            raise BadEntity
    in
    let success starttime =
      let diff = Lang.float_seconds env.env_lang "ping" 
        (Unix.gettimeofday () -. starttime) in
        (match entity with
           | `Mynick _ ->
               Lang.get_msg env.env_lang "plugin_ping_pong_from_me" 
                 [diff]
           | `You ->
               Lang.get_msg env.env_lang "plugin_ping_pong_from_you" 
                 [diff]
           | _ ->
               Lang.get_msg env.env_lang "plugin_ping_pong_from_somebody"
                 [text; diff])
    in
    let starttime = Unix.gettimeofday () in
    let proc t f x _ =
      let reply = match t with
        | `Result ->
            success starttime
        | `Error ->
            (let cond,_,_,_ = Error.parse_error x in
               match cond with
                 | `ERR_FEATURE_NOT_IMPLEMENTED ->
                     success starttime
                 | _ ->
                     Iq.process_error x env
                       entity f 
                       (if text = "" then
                          match env.env_groupchat, entity with
                            | true, `You ->
                                f.resource
                            | _ ->
                                f.string
                        else
                          text)
            )
        | _ -> "?!"
      in
        make_msg out xml reply
    in
    let id = new_id () in
      register_iq_query_callback id proc;
      out (make_iq ~to_ ~xmlns:"jabber:iq:version" ~id ~type_:`Get ())
  with _ ->
    make_msg out xml (Lang.get_msg env.env_lang "invalid_entity" [])
      
let _ =
  register_command "ping" ping
