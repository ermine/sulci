(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open XMPP
open Jid
open Common
open Hooks
open Muc
open Sqlite3

module Sql = Talkers_sql.Make(Sqlgg_sqlite3)

let split_words text =
  Pcre.split ~pat:"[ \t\n]+" text

let talkers db xmpp env kind jid_from nick text =
  match kind with
    | Some Groupchat ->
        if text <> "" then
          let room_env = get_room_env jid_from in
          let room = string_of_jid (bare_jid jid_from) in
          let sentences = 1L in
          let words = Int64.of_int (List.length (split_words text)) in
          let me = 
            if nick = "" && 
              (text = "/me" ||
                  (String.length text > 3 && String.sub text 0 4 = "/me ")) then
                1L else 0L in
          let jid =
            match (Occupant.find jid_from.lresource room_env.occupants).jid
            with
              | None -> ""
              | Some v -> string_of_jid (bare_jid v)
          in
          let test, update =
            if jid = "" then
              let nick = jid_from.lresource in
                (Sql.test_nick db ~nick ~room, Sql.update_by_nick db ~nick ~room)
            else
              (Sql.test_jid db ~jid ~room, Sql.update_by_jid db ~jid ~room)
          in
          let _ =
            match test with
              | None ->
                  Sql.insert_new db ~nick ~jid ~room ~words ~me ~sentences
              | Some _ ->
                  update ~words ~me ~sentences
          in
            ()
    | _ ->
        ()
          
let cmd_talkers db xmpp env kind jid_from text =
  let room = string_of_jid (bare_jid jid_from) in
  let nick = Stringprep.resourceprep text in
  let result = ref [] in
  let max = ref 0 in
  let accu nick words me sentences =
    let len = length_utf8 nick in
      if len > !max then max := len;
      result := (len, nick, words, me, sentences) :: !result
  in
  let select =
    if text = "" then
      Sql.select_talkers_limit ~room
    else
      Sql.select_talkers_by_nick ~room ~nick
  in
    select db accu;
    if !result <> [] then
      let nick_title =
        Lang.get_msg env.env_lang "plugin_talkers_top_header_man" [] in
      let nick_title_len = length_utf8 nick_title in
      let () =
        if nick_title_len > !max then max := nick_title_len in
      let tabs = !max / 8 + 1 in
      let tab = String.make tabs '\t' in
      let data =
        List.fold_left (fun acc (len, nick, words, me, sentences) ->
                          let m = tabs - (len / 8) in
                            (Printf.sprintf "%s%s%Ld\t%Ld\t%Ld\t%.2g"
                               nick
                               (String.sub tab 0 m)
                               words me sentences
                               (Int64.to_float (Int64.div words sentences))
                            ) :: acc
                       ) [] (List.rev !result) in
      let header = Printf.sprintf "%s%s%s\t%s\t%s"
        nick_title
        (Lang.get_msg env.env_lang "plugin_talkers_top_header_words" [])
        (Lang.get_msg env.env_lang "plugin_talkers_top_header_actions" [])
        (Lang.get_msg env.env_lang "plugin_talkers_top_header_sentences" [])
        (Lang.get_msg env.env_lang "plugin_talkers_top_header_average" [])
      in
        env.env_message xmpp kind jid_from
          (String.concat "\n" (header :: (List.rev data)))
    else
      env.env_message xmpp kind jid_from
        (Lang.get_msg env.env_lang "plugin_talkers_no_result" [])
            
let plugin opts =
  let file =
    try List.assoc "path" (List.assoc "db" opts)
    with Not_found ->
      raise
        (PluginError
           "Please specify <db path='/path/talkers.db'/> element in configuration file") in
  let db = db_open file in
    ignore (Sql.create_talkers db);
    ignore (Sql.create_index_talkers_idx db);
    ignore (Sql.create_index_words_idx db);
    Plugin_command.add_commands [("talkers", cmd_talkers db)] opts
    
let _ =
  add_plugin "talkers" plugin

