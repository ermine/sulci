(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open Xmpp
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
              let proc t f x o = 
                match t with
                  | `Result ->
                      if safe_get_attr_s xml "type" = "groupchat" then
                        make_msg o xml
                          (Lang.get_msg env.env_lang "plugin_roulette_bye" [])
                  | `Error ->
                      let err_text =  
                        try 
                          get_cdata ~path:["error"; "text"] x 
                        with _ -> 
                          Lang.get_msg env.env_lang
                            "plugin_roulette_kick_failed" []
                      in
                        make_msg out xml err_text
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
    else
      ()
          
let _ =
  register_command "tryme" roulette
