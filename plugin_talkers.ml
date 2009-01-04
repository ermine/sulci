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
open Sqlite3
open Sqlite_util

let length s = Netconversion.ustring_length `Enc_utf8 s

let split_words text =
  Pcre.split ~pat:"[ \t\n]+" text

let file =
  try trim (Xml.get_attr_s Config.config  ~path:["plugins"; "talkers"] "db")
  with Not_found -> "talkers.db"

let table = "talkers"

let db =
  let db = db_open file in
    create_table file db
      (Printf.sprintf
         "SELECT name FROM SQLITE_MASTER WHERE type='table' AND name='%s'" table)
      (Printf.sprintf
         "CREATE TABLE %s (jid varchar, nick varchar, room varchar, words int, me int, sentences int);
CREATE INDEX talkersidx ON %s (jid, room);
CREATE INDEX words_idx ON %s (words)"
         table table table);
    db
      
let talkers event from xml env out =
  match event with
    | MUC_message (msg_type, nick, text) ->
        if msg_type = `Groupchat then
          let room_s = escape (string_of_jid (bare_jid from)) in
            if text <> "" then
              let room_env = get_room_env from in
                if from.lresource <> room_env.mynick then
                  let words = List.length (split_words text) in
                  let me = 
                    if nick = "" && 
                      ((length text = 3 && text = "/me") ||
                         (length text > 3 && 
                            String.sub text 0 4 = "/me ")) 
                    then
                      1 else 0 in
                  let jid = (Nicks.find from.lresource 
                               room_env.nicks).jid in
                  let cond =
                    match jid with
                      | None -> "nick=" ^ escape from.lresource
                      | Some j ->
                          "jid=" ^ escape (string_of_jid (bare_jid j))
                  in
                  let sql1 = Printf.sprintf
                    "SELECT 1 FROM %s WHERE %s AND room=%s"
                    table cond room_s in
                  let sql2 =
                    Printf.sprintf
                      "UPDATE %s SET words=words+%d, sentences=sentences+%d, me=me+%d WHERE %s AND room=%s" table words  1 me cond room_s in
                  let sql3 =
                    Printf.sprintf
                      "INSERT INTO %s (jid, nick, room, words, me, sentences) VALUES(%s, %s, %s, %d, %d, %d)"
                      table
                      (escape (match jid with
                                 | None -> ""
                                 | Some j -> string_of_jid (bare_jid j)))
                      (escape from.resource)
                      room_s words me 1
                  in
                    ignore (insert_or_update file db sql1 sql2 sql3)
    | _ ->
        ()
          
let cmd_talkers text from xml env out =
  let room_s = escape (string_of_jid (bare_jid from)) in
  let nick = Stringprep.resourceprep text in
  let sql =
    Printf.sprintf 
      "SELECT nick, words, me, sentences FROM %s WHERE room=%s %sORDER BY words DESC, sentences ASC%s"
      table room_s
      (if text <> "" then "AND nick like " ^ escape nick else "")
      (if text = "" then " LIMIT 10" else "")
  in
  let rec aux_step acc stmt =
    match step stmt with
      | Rc.ROW ->
          aux_step 
            ((Data.to_string (column stmt 0),
              Data.to_string (column stmt 1),
              Data.to_string (column stmt 2),
              Data.to_string (column stmt 3)) :: acc) stmt
      | Rc.DONE ->
          if finalize stmt <> Rc.OK then
            exit_with_rc file db sql;
          List.rev acc
      | rc ->
          exit_with_rc file db sql
  in
  let data =
    try
      let stmt = prepare db sql in
        aux_step [] stmt
    with
      | Sqlite3.Error _ ->
          exit_with_rc file db sql
  in
  let header = (
    Lang.get_msg env.env_lang "plugin_talkers_top_header_man" [],
    Lang.get_msg env.env_lang "plugin_talkers_top_header_words" [],
    Lang.get_msg env.env_lang "plugin_talkers_top_header_actions" [],
    Lang.get_msg env.env_lang "plugin_talkers_top_header_sentences" [],
    Lang.get_msg env.env_lang "plugin_talkers_top_header_average" []
  )
  in
  let rec aux_max_len max l =
    match l with
      | [] -> max
      | (nick, _, _, _) :: t -> 
          if length nick > max then
            aux_max_len (length nick) t
          else
            aux_max_len max t
  in
  let (nick_title, _, _, _, _) = header in
  let max_len = aux_max_len (length nick_title / 8) data in
  let tabs = max_len / 8 + 1 in
  let tab = String.make tabs '\t' in
  let rec cycle l acc =
    match l with
      | [] -> String.concat "" (List.rev acc)
      | (nick, words, me, sentences) :: t ->
          let m = tabs - (length nick / 8) in
            cycle t ((Printf.sprintf "%s%s%s\t%s\t%s\t%.2g\n"
                        nick
                        (String.sub tab 0 m)
                        words me sentences
                        (float_of_string words /. float_of_string sentences)
                     ) :: acc)
  in
  let r = cycle data [] in
    if r <> "" then
      let (nick, words, me, sentences, eff) = header in
        make_msg out xml 
          ((Printf.sprintf "\n%s%s%s\t%s\t%s\t%s\n"
              nick
              (String.sub tab 0 (tabs - (length nick / 8)))
              words me sentences eff) ^ 
             r)
    else
      make_msg out xml
        (Lang.get_msg env.env_lang "plugin_talkers_no_result" [])
    
let _ =
  Muc.register_catcher talkers;
  register_command "talkers" cmd_talkers
