(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open XMPP
open Jid
open Common
open Hooks

let msg ?(is_groupchat=(fun _ -> false))  xmpp env text =
  if env.env_check_access env.from "admin" then
    try
      let s = String.index text ' ' in
      let to_ = jid_of_string (String.sub text 0 s) in
      let msg_body = string_after text (s+1) in
        XMPP.send_message xmpp ~jid_to:to_
          ~kind: (if is_groupchat to_ then Groupchat else Chat)
          ~body:msg_body ();
        env.env_message xmpp kind jid_from "done"
    with _ ->
      env.env_message xmpp kind jid_from "?"
  else
    env.env_message xmpp kind jid_from ":-P"
      
let quit xmpp env _text =
  if env.env_check_access env.from "admin" then (
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_admin_quit_bye" []);
    Hooks.quit xmpp
  )
  else
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_admin_quit_no_access" [])
      
let lang_update xmpp env text =
  if env.env_check_access env.from "admin" then
    if text = "" then
      env.env_message xmpp kind jid_from "What language?"
    else
      env.env_message xmpp kind jid_from (Lang.update text)
        
let lr = Pcre.regexp "([a-z][a-z])\\s+(\\w+)(\\s+(.+))?"
  
let lang_admin xmpp env text =
  if env.env_check_access env.from "admin" then
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
          
let sulci_set_rex = Pcre.regexp "([a-zA-Z_-]+) *= *(.+)"
  
let variables = [
  "max_message_length", (fun i -> Hooks.max_message_length := i);
  "msg_limit", (fun i -> Hooks.msg_limit := i);
]
  
let sulci_set xmpp env text =
  if env.env_check_access env.from "admin" then
    try
      let r = Pcre.exec ~rex:sulci_set_rex text in
      let var = Pcre.get_substring r 1
      and value = Pcre.get_substring r 2 in
        try
          let f = List.assoc var variables in
          let newvalue = int_of_string value in
            f newvalue;
            env.env_message xmpp kind jid_from "Done"
        with
          | Failure "int_of_string" ->
              env.env_message xmpp kind jid_from "Bad value: must be integer"
          | Not_found ->
              env.env_message xmpp kind jid_from "Unknown variable"
    with Not_found ->
      env.env_message xmpp kind jid_from "Hm?"
        
let _ =
  register_command "msg" msg;
  register_command "quit" quit;
  register_command "lang_update" lang_update;
  register_command "lang_msgid" lang_admin;
  register_command "sulci_set" sulci_set
