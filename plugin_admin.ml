(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open Xmpp
open Jid
open Error
open Types
open Common
open Hooks

let msg ?(is_groupchat=(fun _ -> false))  text from xml env out =
  if env.env_check_access from "admin" then
    try
      let s = String.index text ' ' in
      let to_ = jid_of_string (String.sub text 0 s) in
      let msg_body = string_after text (s+1) in
        out (make_element "message"
               ["to", to_.string; 
                "type", if is_groupchat to_ then "groupchat" else "chat"]
               [make_simple_cdata "body" msg_body]);
        make_msg out xml "done"
    with _ ->
      make_msg out xml "?"
  else
    make_msg out xml ":-P"
      
let quit text from xml env out =
  if env.env_check_access from "admin" then (
    make_msg out xml (Lang.get_msg env.env_lang "plugin_admin_quit_bye" []);
    Hooks.quit out
  )
  else
    make_msg out xml (Lang.get_msg env.env_lang "plugin_admin_quit_no_access" [])
      
let lang_update text from xml env out =
  if env.env_check_access from "admin" then
    if text = "" then
      make_msg out xml "What language?"
    else
      make_msg out xml (Lang.update text)
        
let lr = Pcre.regexp "([a-z][a-z])\\s+(\\w+)(\\s+(.+))?"
  
let lang_admin text from xml env out =
  if env.env_check_access from "admin" then
    if text = "" then
      make_msg out xml "en msgid string"
    else
      try
        let r = Pcre.exec ~rex:lr text in
        let lang = Pcre.get_substring r 1
        and msgid = Pcre.get_substring r 2
        and str = try Some (Pcre.get_substring r 4) with Not_found -> None 
        in
          try
            Lang.update_msgid lang msgid str;
            make_msg out xml "done"
          with _ ->
            make_msg out xml "problems"
      with _ ->
        make_msg out xml "invalid syntax"
          
(* TODO: it is scratch *)
          
let sulci_set_rex = Pcre.regexp "([a-zA-Z_-]+) *= *(.+)"
  
let variables = [
  "max_message_length", (fun i -> Common.max_message_length := i);
  "msg_limit", (fun i -> Common.msg_limit := i);
]
  
let sulci_set text from xml env out =
  if env.env_check_access from "admin" then
    try
      let r = Pcre.exec ~rex:sulci_set_rex text in
      let var = Pcre.get_substring r 1
      and value = Pcre.get_substring r 2 in
        try
          let f = List.assoc var variables in
          let newvalue = int_of_string value in
            f newvalue;
            make_msg out xml "Done"
        with
          | Failure "int_of_string" ->
              make_msg out xml "Bad value: must be integer"
          | Not_found ->
              make_msg out xml "Unknown variable"
    with Not_found ->
      make_msg out xml "Hm?"
        
let _ =
  register_command "msg" msg;
  register_command "quit" quit;
  register_command "lang_update" lang_update;
  register_command "lang_msgid" lang_admin;
  register_command "sulci_set" sulci_set
