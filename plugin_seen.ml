
(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open Xmpp
open Jid
open Types
open Common
open Hooks
open Hooks
open Muc_types
open Muc
open Nicks
open Sqlite3
open Sqlite_util

exception Break

let file = 
  try trim (Xml.get_attr_s Config.config 
              ~path:["plugins"; "seen"] "db")
  with Not_found -> "sulci_users.db"

let table_greeting = "greeting"
let table_users = "users"

let db =
  let db = Sqlite3.db_open file in
    create_table file db 
      (Printf.sprintf
         "SELECT name FROM SQLITE_MASTER WHERE type='table' AND name='%s'"
         table_greeting)
      (Printf.sprintf
         "CREATE TABLE %s (jid varchar, room varchar, msg varchar);
CREATE INDEX gr_index ON %s (jid, room)"
         table_greeting table_greeting);
    create_table file db
      (Printf.sprintf
         "SELECT name FROM SQLITE_MASTER WHERE type='table' AND name='%s'"
         table_users)
      (Printf.sprintf
         "CREATE TABLE %s (jid varchar, room varchar, nick varchar, last integer, action varchar, reason varchar);
        CREATE INDEX users_index on %s (jid, room)"
         table_users table_users);
    db
      
let cmd_greet =
  Pcre.regexp ~flags:[`DOTALL; `UTF8] "([^\\s]+)\\s+([^\\s]+)\\s+(.+)"
    
let add_greet text from xml env out =
  if env.env_check_access from "admin" then (
    if text <> "" then
      try
        let res = Pcre.exec ~rex:cmd_greet ~pos:0 text in
        let jid = jid_of_string (Pcre.get_substring res 1) in
        let jid_s = escape (string_of_jid (bare_jid jid)) in
        let room = jid_of_string (Pcre.get_substring res 2) in
        let room_s = escape (string_of_jid (bare_jid room)) in
        let greet = escape (Pcre.get_substring res 3) in
        let sql1 = Printf.sprintf "SELECT 1 FROM %s WHERE jid=%s AND room=%s"
          table_greeting jid_s  room_s in
        let sql2 =
          Printf.sprintf "UPDATE %s SET msg=%s WHERE jid=%s AND room=%s"
            table_greeting greet jid_s room_s in
        let sql3 =
          Printf.sprintf
            "INSERT INTO %s (jid, room, msg) VALUES (%s, %s, %s)"
            table_greeting jid_s room_s greet in
          if (insert_or_update file db sql1 sql2 sql3) then
            make_msg out xml
              (Lang.get_msg env.env_lang "plugin_seen_greet_updated" [])
          else
            make_msg out xml (Lang.get_msg env.env_lang "plugin_seen_greet_added" [])
      with Not_found ->
        make_msg out xml (Lang.get_msg env.env_lang "plugin_seen_greet_bad_syntax" [])
  )
  else ()
    
let catch_seen event from xml env out =
  match event with
    | MUC_join item -> (
        let room_s = string_of_jid (bare_jid from) in
        let jid_s =
          match item.jid with
            | None -> ""
            | Some j -> string_of_jid (bare_jid j)
        in
        let sql = Printf.sprintf "SELECT msg FROM %s WHERE jid=%s AND room=%s"
          table_greeting (escape jid_s) (escape room_s) in
          match get_one_row file db sql with
            | None -> ()
            | Some r ->
                out (make_element "message" ["to", room_s; "type", "groupchat"]
                       [make_simple_cdata "body"
                          (Printf.sprintf "[%s] %s"
                             from.resource (string_of_data r.(0)))]
                    )
      )
    | MUC_leave (_, t, reason, item) -> (
        let cond =
          match item.jid with
            | None -> 
                "nick=" ^ escape from.lresource
            | Some j -> 
                "jid=" ^ escape (string_of_jid (bare_jid j))
        in
        let room_s = string_of_jid (bare_jid from) in
        let last = Int32.to_string (Int32.of_float 
                                      (Unix.gettimeofday ())) in
        let action = match t with
          | `Normal -> "left"
          | `Kick -> "kick"
          | `Ban -> "ban"
          | `UnMember -> "unmember"
        in
        let sql1 = Printf.sprintf "SELECT last FROM %s where %s AND room=%s"
          table_users cond (escape room_s) in
        let sql2 =
          Printf.sprintf
            "UPDATE %s SET last=%s, action=%s, reason=%s WHERE %s AND room=%s"
            table_users last (escape action) (escape reason) cond
            (escape room_s) in
        let sql3 =
          Printf.sprintf
            "INSERT INTO %s (jid, room, nick, last, action, reason)
             VALUES (%s,%s,%s, %s, %s, %s)"
            table_users
            (escape (match item.jid with
                       | None -> ""
                       | Some j -> string_of_jid (bare_jid j)))
            (escape room_s)
            (escape from.resource)
            last (escape action) (escape reason) in
          ignore (insert_or_update file db sql1 sql2 sql3)
      )
    | _ -> ()
        
let find_nick (jid:string) nicks =
  let result = ref [] in
    Nicks.iter (fun (nick, item) ->
                  match item.jid with
                    | None -> ()
                    | Some j ->
                        if jid = string_of_jid (bare_jid j) then
                          result := nick :: !result
               ) nicks;
    if !result = [] then raise Not_found else !result
      
let verify_nick nick jid nicks xml env =
  try
    let item = Nicks.find nick nicks in
      match item.jid with
        | None ->
            Lang.get_msg env.env_lang "plugin_seen_is_here" [nick]
        | Some j ->
            if jid = string_of_jid (bare_jid j) then
              Lang.get_msg env.env_lang "plugin_seen_is_here" [nick]
            else if jid = "" then
              Lang.get_msg env.env_lang "plugin_seen_is_here" [nick]
            else
              try let changed = find_nick jid nicks in
                Lang.get_msg env.env_lang "plugin_seen_changed_nick" 
                  [nick; (String.concat ", " changed)]
              with Not_found ->
                Lang.get_msg env.env_lang "plugin_seen_is_not_same" [nick; nick]
  with Not_found ->
    if jid <> "" then
      let changed = find_nick jid nicks in
        Lang.get_msg env.env_lang "plugin_seen_changed_nick" 
          [nick; (String.concat ", " changed)]
    else
      raise Not_found
        
let seen text from xml env out =
  if text = "" then
    make_msg out xml (Lang.get_msg env.env_lang "plugin_seen_whom" [])
  else
    if env.env_groupchat then
      if text = from.resource then
        make_msg out xml (Lang.get_msg env.env_lang "plugin_seen_you" [])
      else
        let room  = from.lnode, from.ldomain in
        let nicks = (get_room_env from).nicks in
        let sql = Printf.sprintf
          "SELECT jid, last, action, reason FROM %s WHERE nick=%s AND room=%s ORDER BY last DESC LIMIT 1"
          table_users
          (escape text)
          (escape (string_of_jid (bare_jid from))) in
        let reply =
          match get_one_row file db sql with
            | Some r -> (
                try
                  verify_nick text (string_of_data r.(0)) nicks xml env
                with Not_found ->
                  let stamp = Int64.to_float (int64_of_data r.(1)) in
                  let diff = 
                    Lang.expand_time env.env_lang "seen"
                      (int_of_float 
                         (Unix.gettimeofday () -. stamp)) in
                    if string_of_data r.(3) = "" then
                      Lang.get_msg env.env_lang 
                        (match string_of_data r.(2) with
                           | "kick" -> "plugin_seen_kicked"
                           | "ban" -> "plugin_seen_banned"
                           | "unmember" -> "plugin_seen_unmembered"
                           | _ -> "plugin_seen_left")
                        [text; diff]
                    else
                      Lang.get_msg env.env_lang 
                        (match string_of_data r.(2) with
                           | "kick" -> 
                               "plugin_seen_kicked_reason"
                           | "ban" -> 
                               "plugin_seen_banned_reason"
                           | "unmember" ->
                               "plugin_seen_unmembered_reason"
                           | _ -> 
                               "plugin_seen_left_reason")
                        [text; diff; string_of_data r.(3)]
              )
            | None -> (
                if Nicks.mem text nicks then
                  Lang.get_msg env.env_lang "plugin_seen_is_here" [text]
                else
                  let result = ref [] in
                    Nicks.iter (fun (nick, item) ->
                                  if item.orig_nick = text then
                                    result := nick :: !result
                               ) nicks;
                    if !result <> [] then
                      if List.mem from.resource !result then
                        Lang.get_msg env.env_lang "plugin_seen_you" []
                      else
                        Lang.get_msg env.env_lang "plugin_seen_changed_nick"
                          [text; String.concat ", " !result]
                    else
                      Lang.get_msg env.env_lang "plugin_seen_never_seen" [text]
              )
        in
          make_msg out xml reply
    else
      make_msg out xml (Lang.get_msg env.env_lang "plugin_seen_not_in_room" [])
            
let _ =
  register_command "greet" add_greet;
  register_command "seen" seen;
  Muc.register_catcher catch_seen
    
