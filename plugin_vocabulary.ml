(*
 * (c) 2004-2009  Anastasia Gornostaeva. <ermine@ermine.pp.ru>
*)

open Jid
open Common
open Hooks
open Plugin_command
open Sqlite3
open Sqlite_util

let total = ref 0

let file = "wtf.db"

let table = "wtf"

let db =
  let db = Sqlite3.db_open file in
    create_table file db
      (Printf.sprintf
         "SELECT name FROM SQLITE_MASTER WHERE type='table' AND name='%s'" table)
      (Printf.sprintf
         "CREATE TABLE %s (stamp integer, nick varchar, luser varchar, lserver varchar, key varchar, value varchar);
CREATE INDEX dfnidx ON %s (key)"
         table  table);
    let t =
      match get_one_row file db
        (Printf.sprintf "SELECT count(*) FROM %s" table) with
          | None -> 0
          | Some r -> Int64.to_int (int64_of_data r.(0)) in
      total := t;
      db
        
let dfn_re = Pcre.regexp ~flags:[`DOTALL; `UTF8] "([^=]+)\\s*=\\s*(.*)"


let get_real_jid from =
  let cond = " AND nick=" ^ escape from.lresource ^
    " AND luser=" ^ escape from.lnode ^
    " AND lserver=" ^ escape from.ldomain in
    from.lresource, from.lnode, from.ldomain, cond

let dfn ?(get_real_jid=get_real_jid) xmpp env text =
  let key, value=
    try
      let res = Pcre.exec ~rex:dfn_re text in
      let key = Pcre.get_substring res 1 in
      let value = try Pcre.get_substring res 2 with Not_found -> "" in
        key, value
    with Not_found ->
      "", ""
  in
    if key = "" then
      env.env_message xmpp kind jid_from
        (Lang.get_msg env.env_lang "plugin_vocabulary_invalid_syntax" [])
    else
      let nick, lnode, ldomain, cond =
        if env.env_groupchat then
          get_real_jid env.from
        else
          let cond = " AND luser=" ^ escape env.from.lnode ^ 
            " AND lserver=" ^ escape env.from.ldomain in
            env.from.lnode, env.from.lnode, env.from.ldomain, cond
      in
      let sql = Printf.sprintf
        "SELECT value FROM %s WHERE key=%s %s" table (escape key) cond in
        match get_one_row file db sql with
          | Some r ->
              if value = (string_of_data r.(0)) then
                env.env_message xmpp kind jid_from
                  (Lang.get_msg env.env_lang "plugin_vocabulary_dfn_again" [])
              else if value = "" then (
                simple_exec file db
                  (Printf.sprintf
                     "DELETE FROM %s WHERE key=%s %s" table (escape key) cond);
                decr total;
                env.env_message xmpp kind jid_from
                  (Lang.get_msg env.env_lang "plugin_vocabulary_removed" [])
              )
              else
                let stamp =
                  Int32.to_string (Int32.of_float  (Unix.gettimeofday ())) in
                  simple_exec file db
                    (Printf.sprintf
                       "UPDATE %s SET stamp=%s, nick=%s, value=%s WHERE key=%s %s"
                       table stamp (escape nick) (escape value) (escape key) cond);
                  env.env_message xmpp kind jid_from
                    (Lang.get_msg env.env_lang "plugin_vocabulary_replaced" [])
          | None ->
              if value <> "" then
                let stamp = Int32.to_string (Int32.of_float 
                                               (Unix.gettimeofday ())) in
                  simple_exec file db
                    (Printf.sprintf
                       "INSERT INTO %s (stamp,nick,luser,lserver,key,value)
  VALUES(%s,%s,%s,%s,%s,%s)"
                       table stamp (escape nick) (escape lnode) (escape ldomain)
                       (escape key) (escape value));
                  incr total;
                  env.env_message xmpp kind jid_from
                    (Lang.get_msg env.env_lang "plugin_vocabulary_recorded" [])
              else
                env.env_message xmpp kind jid_from
                  (Lang.get_msg env.env_lang
                     "plugin_vocabulary_nothing_to_remove" [])

let wtf xmpp env text =
  if text = "" then
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_vocabulary_invalid_syntax" [])
  else
    let key =
      try
        let q = String.index text '?' in
          String.sub text 0 q
      with Not_found -> text in
    let sql = Printf.sprintf
      "SELECT nick, value FROM %s WHERE key=%s ORDER BY stamp DESC LIMIT 1"
      table (escape key) in
      match get_one_row file db sql with
        | Some r ->
            env.env_message xmpp kind jid_from
              (Lang.get_msg env.env_lang "plugin_vocabulary_answer"
                 [string_of_data r.(0); key; string_of_data r.(1)])
        | None ->
            env.env_message xmpp kind jid_from
              (Lang.get_msg env.env_lang "plugin_vocabulary_not_found" [])
              
let output_records xmpp env sql =
  let rec aux_acc i acc stmt =
    match get_row stmt with
      | Some row ->
          let str = Printf.sprintf "%d) %s"
            i
            (Lang.get_msg env.env_lang "plugin_vocabulary_answer"
               [Data.to_string row.(0);
                Data.to_string row.(1);
                Data.to_string row.(2)])
          in
            aux_acc (i+1) (str :: acc) stmt
      | None ->
          if acc = [] then "" else String.concat "\n" (List.rev acc)
  in
    try
      let stmt = prepare db sql in
      let reply = aux_acc 1 [] stmt in
        if reply = "" then
          env.env_message xmpp kind jid_from
            (Lang.get_msg env.env_lang "plugin_vocabulary_not_found" [])
        else
          env.env_message xmpp kind jid_from reply
    with Sqlite3.Error _ ->
      exit_with_rc file db sql

let wtfall xmpp env text =
  if text = "" then
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_vocabulary_invalid_syntax" [])
  else
    let key =
      try
        let q = String.index text '?' in
          String.sub text 0 q
      with Not_found -> text in
    let sql =
      Printf.sprintf
        "SELECT nick, key, value FROM %s WHERE key=%s ORDER BY stamp"
        table (escape key)
    in
      output_records xmpp env sql
      
let wtfrand xmpp env text =
  let key = trim(text) in
    if key = "" then
      let rand = string_of_int (Random.int (!total)) in
      let sql = Printf.sprintf
        "SELECT nick, key, value FROM %s LIMIT %s,1" table rand in
      let reply =
        match get_one_row file db sql with
          | None ->
              Lang.get_msg env.env_lang "plugin_vocabulary_db_is_empty" []
          | Some r ->
              Lang.get_msg env.env_lang "plugin_vocabulary_answer"
                [string_of_data r.(0);
                 string_of_data r.(1);
                 string_of_data r.(2);]
      in          
        env.env_message xmpp kind jid_from reply
    else
      let sql = Printf.sprintf
        "SELECT count(*) FROM %s WHERE key=%s" table (escape key) in
        match get_one_row file db sql with
          | Some r -> (
              let sql = Printf.sprintf
                "SELECT nick, value FROM %s WHERE key=%s LIMIT %d,1"
                table (escape key)
                (Random.int (Int64.to_int (int64_of_data r.(0)))) in
                match get_one_row file db sql with
                  | Some q ->
                      env.env_message xmpp kind jid_from
                        (Lang.get_msg env.env_lang "plugin_vocabulary_answer"
                           [string_of_data q.(0); key; string_of_data q.(1)])
                  | None ->
                      env.env_message xmpp kind jid_from
                        (Lang.get_msg env.env_lang
                           "plugin_vocabulary_not_found" [])
            )
          | None ->
              env.env_message xmpp kind jid_from
                (Lang.get_msg env.env_lang "plugin_vocabulary_not_found" [])
                
let wtfcount xmpp env text =
  let key = trim(text) in
  let sql =
    if key = "" then
      Printf.sprintf "SELECT count(*) FROM %s" table
    else
      Printf.sprintf "SELECT count(*) FROM %s WHERE key=%s" table (escape key)
  in
  let i =
    match get_one_row file db sql with
      | None -> 0
      | Some r -> Int64.to_int (int64_of_data r.(0))
  in
    if key = "" then
      total := i;
    if i > 0 then
      env.env_message xmpp kind jid_from
        (Lang.get_msg env.env_lang "plugin_vocabulary_records"
           [string_of_int i])
    else
      env.env_message xmpp kind jid_from
        (Lang.get_msg env.env_lang
           "plugin_vocabulary_db_is_empty" [])
        
let wtffind xmpp env text =
  if text = "" then
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_vocabulary_invalid_syntax" [])
  else
    let sql = Printf.sprintf
      "SELECT nick, key, value FROM %s WHERE key LIKE %s OR value LIKE %s"
      table (escape text) (escape text)
    in
      output_records xmpp env sql


let plugin opts =
  add_commands [("dfn", dfn);
                ("wtf", wtf);
                ("wtfall", wtfall);
                ("wtfrand", wtfrand);
                ("wtfcount", wtfcount);
                ("wtffind", wtffind)] opts

let _ =
  add_plugin "vocabulary" plugin
