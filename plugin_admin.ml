(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open XMPP
open Jid
open Common
open Hooks
open Plugin_command

let msg xmpp env kind jid_from text =
  if env.env_check_access jid_from "admin" then
    try
      let s = String.index text ' ' in
      let to_ = jid_of_string (String.sub text 0 s) in
      let msg_body = string_after text (s+1) in
        XMPP.send_message xmpp ~jid_to:to_
          ~kind:Chat ~body:msg_body ();
        env.env_message xmpp kind jid_from "done"
    with _ ->
      env.env_message xmpp kind jid_from "?"
  else
    env.env_message xmpp kind jid_from ":-P"
      
let quit xmpp env kind jid_from text =
  if env.env_check_access jid_from "admin" then (
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_admin_quit_bye" []);
    XMPP.send_presence xmpp ~kind:Unavailable ~status:text ();
    XMPP.close_stream xmpp;
  )
  else
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_admin_quit_no_access" [])
      
let lang_update xmpp env kind jid_from text =
  if env.env_check_access jid_from "admin" then
    if text = "" then
      env.env_message xmpp kind jid_from "What language?"
    else
      env.env_message xmpp kind jid_from (Lang.update text)
        
let lr = Pcre.regexp "([a-z][a-z])\\s+(\\w+)(\\s+(.+))?"
  
let lang_admin xmpp env kind jid_from text =
  if env.env_check_access jid_from "admin" then
    if text = "" then
      env.env_message xmpp kind jid_from "en msgid string"
    else
      try
        let r = Pcre.exec ~rex:lr text in
        let lang = Pcre.get_substring r 1
        and msgid = Pcre.get_substring r 2
        and str = try Some (Pcre.get_substring r 4) with Not_found -> None 
        in
          try
            Lang.update_msgid lang msgid str;
            env.env_message xmpp kind jid_from "done"
          with _ ->
            env.env_message xmpp kind jid_from "problems"
      with _ ->
        env.env_message xmpp kind jid_from "invalid syntax"
          
(* TODO: it is scratch *)
          
let plugin opts =
  add_commands ["msg", msg;
                "quit", quit;
                "lang_update", lang_update;
                "lang_msgid", lang_admin] opts

let _ =
    add_plugin "admin" plugin
