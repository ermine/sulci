(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open StanzaError
open XMPP
open Jid
open Types
open Common
open Hooks
open Muc_types
open Muc
open Nicks
  
let r = Random.self_init ()

let roulette text from xml env out =
  if text <> "" then
    make_msg out xml 
      (Lang.get_msg env.env_lang "plugin_roulette_syntax_error" [])
  else
    if env.env_groupchat then
      let room_env = get_room_env from in
      let myitem = Nicks.find room_env.mynick room_env.nicks in
        if myitem.role = `Moderator then
          let item = Nicks.find from.lresource room_env.nicks in
            if item.role = `Moderator then
              make_msg out xml
                (Lang.get_msg env.env_lang "plugin_roulette_not_allowed" [])
            else if Random.int 10 = 1 then
              let id = new_id () in
              let reason = 
                Lang.get_msg env.env_lang "plugin_roulette_kick_reason" [] in
              let proc t _f x o = 
                match t with
                  | IqResult _el ->
                      if safe_get_attr_value "type" (get_attrs xml) =
                        "groupchat" then
                          make_msg o xml
                            (Lang.get_msg env.env_lang "plugin_roulette_bye" [])
                  | IqError err ->
                      make_msg out xml (if err.err_text = "" then
                                          Lang.get_msg env.env_lang
                                            "plugin_roulette_kick_failed" []
                                        else err.err_text)
                  | _ ->
                      ()
              in
                register_iq_query_callback id proc;
                out (Muc.kick ~reason id (from.lnode, from.ldomain)
                       from.resource);
            else
              make_msg out xml
                (Lang.get_msg env.env_lang "plugin_roulette_next_time" [])
        else
          make_msg out xml
            (Lang.get_msg env.env_lang "plugin_roulette_not_allowed" [])
          
let _ =
  register_command "tryme" roulette
