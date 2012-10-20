(*
 * (c) 2004-2012 Anastasia Gornostaeva
 *)

open JID
open Common
open Hooks
open Plugin_command
open XMPPClient

let msg xmpp env kind jid_from text =
  try
    let s = String.index text ' ' in
    let jid_to = JID.of_string (String.sub text 0 s) in
    let body = string_after text (s+1) in
      env.env_message xmpp (Some Chat) jid_to body;
      env.env_message xmpp kind jid_from "done"
  with _ ->
    env.env_message xmpp kind jid_from "?"
      
let quit xmpp env kind jid_from text =
  env.env_message xmpp kind jid_from
    (Lang.get_msg env.env_lang "plugin_admin_quit_bye" []);
  XMPPClient.send_presence xmpp ~kind:Unavailable ~status:text ();
  XMPPClient.close_stream xmpp;
  Pervasives.exit 0
      
let lang_update xmpp env kind jid_from text =
  if text = "" then
    env.env_message xmpp kind jid_from "What language?"
  else
    env.env_message xmpp kind jid_from (Lang.update text)
        
let lr = Pcre.regexp "([a-z][a-z])\\s+(\\w+)(\\s+(.+))?"
  
let lang_admin xmpp env kind jid_from text =
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
          
let shell xmpp env kind jid_from text =
  if text = "" then
    env.env_message xmpp kind jid_from "shell? yeah!"
  else
    let in_ch = Unix.open_process_in text in
    let rec aux_read acc =
      let line = try Some (input_line in_ch) with End_of_file -> None in
        match line with
          | Some v -> aux_read (v :: acc)
          | None -> close_in in_ch; List.rev acc
    in
    let lines = aux_read [] in
      if lines <> [] then
        env.env_message xmpp kind jid_from (String.concat "\n" lines)

let plugin opts =
  add_for_token
    (fun _opts xmpp ->
       add_commands xmpp ["msg", msg;
                          "quit", quit;
                          "lang_update", lang_update;
                          "lang_msgid", lang_admin;
                          "sh", shell
                         ] opts
    )

let () =
  Plugin.add_plugin "admin" plugin
