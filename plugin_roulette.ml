(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open StanzaError
open XMPP
open Jid
open Hooks
open Muc
open Xep_muc
  
let r = Random.self_init ()

let roulette xmpp env kind jid_from text =
  if text <> "" then
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_roulette_syntax_error" [])
  else
    let room_env = get_room_env jid_from in
    let myitem = Occupant.find room_env.mynick room_env.occupants in
      if myitem.role = RoleModerator then
        let item = Occupant.find jid_from.lresource room_env.occupants in
          if item.role = RoleModerator then
            env.env_message xmpp kind jid_from
              (Lang.get_msg env.env_lang "plugin_roulette_not_allowed" [])
          else if Random.int 10 = 1 then
            let callback  ev _jidfrom _jidto _lang () =
              match ev with
                | IQResult el ->
                    env.env_message xmpp kind jid_from 
                      (Lang.get_msg env.env_lang "plugin_roulette_bye" [])
                | IQError err ->
                    env.env_message xmpp kind jid_from
                      (if err.err_text = "" then
                         Lang.get_msg env.env_lang
                           "plugin_roulette_kick_failed" []
                       else err.err_text)
            in
            let reason = 
              Lang.get_msg env.env_lang "plugin_roulette_kick_reason" [] in
              Muc.kick xmpp ~reason jid_from jid_from.lresource
                callback
          else
            env.env_message xmpp kind jid_from
              (Lang.get_msg env.env_lang "plugin_roulette_next_time" [])
      else
        env.env_message xmpp kind jid_from
          (Lang.get_msg env.env_lang "plugin_roulette_not_allowed" [])
          
let plugin opts =
    Plugin_command.add_commands [("roulette", roulette)] opts

let _ =
  add_plugin "roulette" plugin
