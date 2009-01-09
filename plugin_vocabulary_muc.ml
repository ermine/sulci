(*
 * (c) 2004-2009  Anastasia Gornostaeva. <ermine@ermine.pp.ru>
*)

open Jid
open Types
open Common
open Muc_types
open Muc
open Nicks
open Sqlite_util

let get_real_jid from =
  let room_env = get_room_env from in
  let item = Nicks.find from.lresource room_env.nicks in
    match item.jid with
      | Some jid ->
          let cond = " AND luser=" ^ escape jid.lnode ^
            " AND lserver=" ^ escape jid.ldomain in
            from.lresource, jid.lnode, jid.ldomain, cond
      | None ->
          let cond = " AND nick=" ^ escape from.lresource ^
            " AND luser=" ^ escape from.lnode ^
            " AND lserver=" ^ escape from.ldomain in
            from.lresource, from.lnode, from.ldomain, cond
              
let wtfremove_re = Pcre.regexp ~flags:[`DOTALL; `UTF8] "([^=]+)(\\s*=\\s*(.*))?"

let wtfremove text from xml env out =
  let key, value =
    try
      let res = Pcre.exec ~rex:wtfremove_re text in
      let key = Pcre.get_substring res 1 in
      let value = try Pcre.get_substring res 3 with Not_found -> "" in
        key, value
    with Not_found ->
      ("", "") in
    if key = "" then
      make_msg out xml 
        (Lang.get_msg env.env_lang "plugin_vocabulary_invalid_syntax" [])
    else
      let cond =
        if env.env_groupchat then
          let room_env = get_room_env from in
          let item = Nicks.find from.lresource room_env.nicks in
            if item.role = `Moderator then
              ""
            else
              match item.jid with
                | None ->
                    " AND nick=" ^ escape from.lresource ^
                      " AND luser=" ^ escape from.lnode ^
                      " AND lserver=" ^ escape from.ldomain
                | Some j ->
                    " AND luser=" ^ escape j.lnode ^
                      " AND lserver=" ^ escape j.ldomain
        else
          " AND luser=" ^ escape from.lnode ^
            " AND lserver=" ^ escape from.ldomain
      in
      let sql cmd =
        if value = "" then
          Printf.sprintf
            "%s FROM %s WHERE key=%s %s" cmd table (escape key) cond
        else
          Printf.sprintf
            "%s FROM %s WHERE key=%s AND value=%s %s"
            cmd table (escape key) (escape value) cond
      in
        let records =
          match get_one_row file db (sql "SELECT count(*)") with
            | None -> 0
            | Some r -> Int64.to_int (int64_of_data r.(0))
        in
          if records > 0 then (
            simple_exec file db (sql "DELETE");
            make_msg out xml (Lang.get_msg env.env_lang
                                "plugin_vocabulary_removed" [])
          )
          else
            make_msg out xml (Lang.get_msg env.env_lang
                                "plugin_vocabulary_nothing_to_remove" [])

let _ =
  Hooks.register_command "dfn" (Plugin_vocabulary.dfn ~get_real_jid);
  Hooks.register_command "wtfremove" wtfremove
