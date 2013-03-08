(*
 * (c) 2004-2013 Anastasia Gornostaeva
 *)

open JID
open Common
open Hooks
open Muc
open XMPPClient

open Sqlite3
module Sql = Talkers_sql.Make(Sqlgg_sqlite3)

let split_words text =
  Pcre.split ~pat:"[ \t\n]+" text

let muc_talkers db muc_context xmpp env kind jid_from nick text =
  match kind with
    | Some Groupchat ->
        if text <> "" then
          let room_env = get_room_env muc_context jid_from in
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
                  let nick = jid_from.lresource in
                    Sql.insert_new db ~nick ~jid ~room ~words ~me ~sentences
              | Some _ ->
                  update ~words ~me ~sentences
          in
            ()
    | _ ->
        ()
          
let cmd_talkers db muc_context xmpp env kind jid_from text =
  let room = string_of_jid (bare_jid jid_from) in
  let nick = JID.resourceprep text in
  let accu nick words me sentences (max_len, result) =
    let len = length_utf8 nick in
      (max max_len len, (len, nick, words, me, sentences) :: result)
  in
  let select =
    if text = "" then
      Sql.Fold.select_talkers_limit ~room
    else
      Sql.Fold.select_talkers_by_nick ~room ~nick
  in
  let max, result = select db accu (0, []) in
    if result <> [] then
      let nick_title =
        Lang.get_msg env.env_lang "plugin_talkers_top_header_man" [] in
      let nick_title_len = length_utf8 nick_title in
      let max = Pervasives.max nick_title_len max in
      let tabs = max / 8 + 1 in
      let tab = String.make tabs '\t' in
      let data =
        List.fold_left
          (fun acc (len, nick, words, me, sentences) ->
             let m = tabs - (len / 8) in
               (Printf.sprintf "%s%s%Ld\t%Ld\t%Ld\t%.2g"
                  nick
                  (String.sub tab 0 m)
                  words me sentences
                  ((Int64.to_float words) /. (Int64.to_float sentences))
               ) :: acc
          ) [] (List.rev result) in
      let header = Printf.sprintf "%s%s%s\t%s\t%s\t%s"
        nick_title
        (let tabs = max / 8 + 1 in String.make tabs '\t')
        (Lang.get_msg env.env_lang "plugin_talkers_top_header_words" [])
        (Lang.get_msg env.env_lang "plugin_talkers_top_header_actions" [])
        (Lang.get_msg env.env_lang "plugin_talkers_top_header_sentences" [])
        (Lang.get_msg env.env_lang "plugin_talkers_top_header_average" [])
      in
        env.env_message xmpp kind jid_from
          ("\n" ^ (String.concat "\n" (header :: (List.rev data))))
    else
      env.env_message xmpp kind jid_from
        (Lang.get_msg env.env_lang "plugin_talkers_no_result" [])
            
let plugin opts =
  let file =
    try List.assoc "file" (List.assoc "db" opts)
    with Not_found -> "talkers.db" in
    Muc.add_for_muc_context
      (fun muc_context xmpp ->
         (* get spool path *)
         let db = db_open file in
           ignore (Sql.create_talkers db);
           ignore (Sql.create_index_talkers_idx db);
           ignore (Sql.create_index_words_idx db);
           Muc.add_hook_conversation muc_context (muc_talkers db);
           Plugin_command.add_commands xmpp
             ["talkers", cmd_talkers db muc_context] opts
      )
    
let () =
  Plugin.add_plugin "talkers" plugin
