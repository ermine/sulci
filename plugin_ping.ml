(*
 * (c) 2004-2008 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open Xmpp
open Jid
open Common
open Types
open Xmpp
open Hooks

let ping text event from xml (out:element -> unit) =
  try
    let entity = get_entity text event from in
    let to_ =
      match entity with
        | `Mynick nick
        | `Nick nick ->
            string_of_jid {from with resource = nick}
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
      let diff = Lang.float_seconds ~xml "ping" 
        (Unix.gettimeofday () -. starttime) in
        (match entity with
           | `Mynick _ ->
               Lang.get_msg ~xml "plugin_ping_pong_from_me" 
                 [diff]
           | `You ->
               Lang.get_msg ~xml "plugin_ping_pong_from_you" 
                 [diff]
           | _ ->
               Lang.get_msg ~xml "plugin_ping_pong_from_somebody"
                 [text; diff])
    in
    let starttime = Unix.gettimeofday () in
    let proc e f x _ =
      let reply = match e with
        | Iq (_, `Result, _) ->
            success starttime
        | Iq (_, `Error, _) ->
            (let cond,_,_,_ = Error.parse_error x in
               match cond with
                 | `ERR_FEATURE_NOT_IMPLEMENTED ->
                     success starttime
                 | _ ->
                     Iq.process_error x (Lang.get_lang xml) 
                       entity f 
                       (if text = "" then
                          match event, entity with
                            | MUC_message _, `You ->
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
      register_handle (Id (id, proc));
      out (make_iq ~to_ ~xmlns:"jabber:iq:version" ~id ~type_:`Get ())
  with _ ->
    make_msg out xml (Lang.get_msg ~xml "invalid_entity" [])
      
let _ =
  register_handle (Command ("ping", ping))
