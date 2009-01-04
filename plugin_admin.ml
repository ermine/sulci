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
open Muc_types
open Muc
open Nicks

let msg text from xml env out =
  if check_access from "admin" then
    let s = String.index text ' ' in
    let to_ = jid_of_string (String.sub text 0 s) in
    let msg_body = string_after text (s+1) in
      out (make_element "message"
             ["to", to_.string; 
              "type", if env.env_groupchat then "groupchat" else "chat"]
             [make_simple_cdata "body" msg_body])
  else
    make_msg out xml ":-P"
      
let quit text from xml env out =
  if check_access from "admin" then (
    make_msg out xml (Lang.get_msg env.env_lang "plugin_admin_quit_bye" []);
    Hooks.quit out
  )
  else
    make_msg out xml (Lang.get_msg env.env_lang "plugin_admin_quit_no_access" [])
      
let join_rex = Pcre.regexp "([^\\s]+)(\\s+(.*))?"
  
let join text from xml env out =
  if check_access from "admin" then
    try
      let r = Pcre.exec ~rex:join_rex text in
      let room = jid_of_string (Pcre.get_substring r 1) in
        if not (is_groupchat room) then
          let nick = try Pcre.get_substring r 3 with Not_found ->
            trim (get_cdata Config.config ~path:["jabber"; "user"]) in
          let lang = Lang.deflang in
          let chatlog = true in
          let filter = "cerberus" in
            Muc.register_room ~lang ~filter nick room;
            Muc.add_room room nick lang chatlog filter;
            out (Muc.join_room nick (room.lnode, room.ldomain))
        else
          make_msg out xml "again?"
    with _ ->
      ()
  else
    make_msg out xml (Lang.get_msg env.env_lang "plugin_admin_join_no_access" [])
      
let leave_rex = Pcre.regexp "([^\\s]+)(\\s+(.*))?"
  
let leave text from xml env out =
  if check_access from "admin" then
    try
      let r = Pcre.exec ~rex:leave_rex text in
      let room_s = Pcre.get_substring r 1
      and reason = 
        try Some (Pcre.get_substring r 3) with Not_found -> None in
      let room = jid_of_string room_s in
        if is_groupchat room then
          out (Muc.leave_room (room.lnode, room.ldomain) ?reason)
        else
          raise Not_found
    with exn ->
      make_msg out xml "hmm?"
        
        
let invite_rex = Pcre.regexp "([^\\s]+)(\\s+(.*))?"
  
let invite text from xml env out =
  if check_access from "admin" then
    try
      let r = Pcre.exec ~rex:leave_rex text in
      let who = Pcre.get_substring r 1
      and room_dst = try Pcre.get_substring r 3 with Not_found -> "" in
        if room_dst = "" && env.env_groupchat then
          let _ = jid_of_string who in
            out (Muc.invite (from.lnode, from.ldomain) who)
        else
          let rjid = jid_of_string room_dst in
            if rjid.lresource = "" && env.env_groupchat then
              let room_env = get_room_env from in
                try
                  match (Nicks.find who room_env.nicks).jid with
                    | Some j ->
                        out (Muc.invite (rjid.lnode, rjid.ldomain)
                               (j.lnode ^ "@" ^ j.ldomain))
                    | None ->
                        raise Not_found
                with Not_found ->
                  let _ = jid_of_string who in
                    out (Muc.invite (rjid.lnode, rjid.ldomain) 
                           who)
            else
              raise Not_found
    with exn ->
      make_msg out xml "hmm?"
        
let lang_update text from xml env out =
  if check_access from "admin" then
    if text = "" then
      make_msg out xml "What language?"
    else
      make_msg out xml (Lang.update text)
        
let lr = Pcre.regexp "([a-z][a-z])\\s+(\\w+)(\\s+(.+))?"
  
let lang_admin text from xml env out =
  if check_access from "admin" then
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
  if check_access from "admin" then
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
  register_command "join" join;
  register_command "leave" leave;
  register_command "invite" invite;
  register_command "lang_update" lang_update;
  register_command "lang_msgid" lang_admin;
  register_command "sulci_set" sulci_set
