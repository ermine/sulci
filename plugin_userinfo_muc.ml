(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Types
open Jid
open Common
open Muc_types
open Muc
open Nicks

let status text from xml env out =
  if env.env_groupchat then
    let entity = if text = "" then from.lresource else
      Stringprep.resourceprep text in
      (try
         let item = Nicks.find entity (get_room_env from).nicks in
           make_msg out xml ((if item.status = "" then ""
                              else item.status ^ " ") ^
                               "[" ^ (match item.show with
                                        | `Online -> "online"
                                        | `Away -> "away"
                                        | `DND -> "dnd"
                                        | `Chat -> "free for chat"
                                        | `XA -> "xa") ^ "]")
       with _ ->
         make_msg out xml 
           (Lang.get_msg env.env_lang "plugin_userinfo_status_whose" []))

let _ =
  Hooks.register_command"status" status
