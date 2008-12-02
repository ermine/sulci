(*
 * (c) 2004-2008  Anastasia Gornostaeva. <ermine@ermine.pp.ru>
*)

open Xml
open Xmpp
open Jid
open Common
open Types
open Nicks
open Sqlite3
open Sqlite_util

let total = ref 0

let file =
  try trim (Xml.get_attr_s Config.config 
              ~path:["plugins"; "vocabulary"] "db")
  with Not_found -> "wtf.db"

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
        
let dfn text event from xml out =
  try
    let eq = String.index text '=' in
    let key = 
      let key' = trim (String.sub text 0 eq) in
        if key' = "" then raise Not_found
        else
          key'
    in
    let value = trim (string_after text (eq+1)) in
    let nick, lnode, ldomain, cond =
      match event with
        | MUC_message _ ->
            let room = (from.lnode, from.ldomain) in
            let nicks = (GroupchatMap.find room !groupchats).nicks in
              (match (Nicks.find from.lresource nicks).jid with
                 | Some jid ->
                     let cond = " AND luser=" ^ escape jid.lnode ^
                       " AND lserver=" ^ escape jid.ldomain in
                       from.lresource, jid.lnode, jid.ldomain, cond
                 | None ->
                     let cond = " AND nick=" ^ escape from.lresource ^
                       " AND luser=" ^ escape from.lnode ^
                       " AND lserver=" ^ escape from.ldomain in
                       from.lresource, from.lnode, from.ldomain, cond
              )
        | _ ->
            let cond = " AND luser=" ^ escape from.lnode ^ 
              " AND lserver=" ^ escape from.ldomain in
              from.lnode, from.lnode, from.ldomain, cond
    in
    let sql = Printf.sprintf
      "SELECT value FROM %s WHERE key=%s %s" table (escape key) cond in
      match get_one_row file db sql with
        | Some r ->
            if value = (string_of_data r.(0)) then
              make_msg out xml 
                (Lang.get_msg ~xml "plugin_vocabulary_dfn_again" [])
            else if value = "" then (
              simple_exec file db
                (Printf.sprintf
                   "DELETE FROM %s WHERE key=%s %s" table (escape key) cond);
              decr total;
              make_msg out xml 
                (Lang.get_msg ~xml "plugin_vocabulary_removed" [])
            )
            else
              let stamp =
                Int32.to_string (Int32.of_float  (Unix.gettimeofday ())) in
                simple_exec file db
                  (Printf.sprintf
                     "UPDATE %s SET stamp=%s, nick=%s, value=%s WJERE key=%s %s"
                     table stamp (escape nick) (escape value) (escape key) cond);
                make_msg out xml
                  (Lang.get_msg ~xml "plugin_vocabulary_replaced" [])
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
                make_msg out xml 
                  (Lang.get_msg ~xml "plugin_vocabulary_recorded" [])
            else
              make_msg out xml
                (Lang.get_msg ~xml "plugin_vocabulary_nothing_to_remove" [])
  with Not_found ->
    make_msg out xml 
      (Lang.get_msg ~xml "plugin_vocabulary_invalid_syntax" [])
      
let wtf text event from xml out =
  if text = "" then
    make_msg out xml 
      (Lang.get_msg ~xml "plugin_vocabulary_invalid_syntax" [])
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
            make_msg out xml 
              (Lang.get_msg ~xml "plugin_vocabulary_answer"
                 [string_of_data r.(0); key; string_of_data r.(1)])
        | None ->
            make_msg out xml 
              (Lang.get_msg ~xml "plugin_vocabulary_not_found" [])
              
let output_records sql xml out =
  let rec aux_acc i acc stmt =
    match step stmt with
      | Rc.ROW ->
          let str = Printf.sprintf "%d) %s"
            i
            (Lang.get_msg ~xml "plugin_vocabulary_answer"
               [Data.to_string (column stmt 0);
                Data.to_string (column stmt 1);
                Data.to_string (column stmt 2)])
          in
            aux_acc (i+1) (str :: acc) stmt
      | Rc.DONE -> 
          if acc = [] then "" else String.concat "\n" (List.rev acc)
      | _ -> exit_with_rc file db sql
  in
    try
      let stmt = prepare db sql in
      let reply = aux_acc 1 [] stmt in
        if reply = "" then
          make_msg out xml 
            (Lang.get_msg ~xml "plugin_vocabulary_not_found" [])
        else
          make_msg out xml reply
    with Sqlite3.Error _ ->
      exit_with_rc file db sql

let wtfall text event from xml out =
  if text = "" then
    make_msg out xml 
      (Lang.get_msg ~xml "plugin_vocabulary_invalid_syntax" [])
  else
    let key =
      try
        let q = String.index text '?' in
          String.sub text 0 q
      with Not_found -> text in
    let sql =
      Printf.sprintf "SELECT nick, key, value FROM %s WHERE key=%s ORDER BY stamp"
        table (escape key)
    in
      output_records sql xml out
      
let wtfrand text event from xml out =
  let key = trim(text) in
    if key = "" then
      let rand = string_of_int (Random.int (!total)) in
      let sql = Printf.sprintf
        "SELECT nick, key, value FROM %s LIMIT %s,1" table rand in
      let reply =
        match get_one_row file db sql with
          | None ->
              Lang.get_msg ~xml "plugin_vocabulary_db_is_empty" []
          | Some r ->
              Lang.get_msg ~xml "plugin_vocabulary_answer"
                [string_of_data r.(0);
                 string_of_data r.(1);
                 string_of_data r.(2);]
      in          
        make_msg out xml reply
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
                      make_msg out xml 
                        (Lang.get_msg ~xml "plugin_vocabulary_answer"
                           [string_of_data q.(0); key; string_of_data q.(1)])
                  | None ->
                      make_msg out xml 
                        (Lang.get_msg ~xml "plugin_vocabulary_not_found" [])
            )
          | None ->
              make_msg out xml 
                (Lang.get_msg ~xml "plugin_vocabulary_not_found" [])
                
let wtfcount text event from xml out =
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
      make_msg out xml (Lang.get_msg ~xml "plugin_vocabulary_records"
                          [string_of_int i])
    else
      make_msg out xml (Lang.get_msg ~xml "plugin_vocabulary_db_is_empty" [])
        
let wtffind text event from xml out =
  if text = "" then
    make_msg out xml 
      (Lang.get_msg ~xml "plugin_vocabulary_invalid_syntax" [])
  else
    let sql = Printf.sprintf
      "SELECT nick, key, value FROM %s WHERE key LIKE %s OR value LIKE %s"
      table (escape text) (escape text)
    in
      output_records sql xml out

let _ =
  List.iter (fun (command, callback) ->
               Hooks.register_handle (Hooks.Command (command, callback)))
    ["wtfall", wtfall;
     "wtfrand", wtfrand;
     "wtfcount", wtfcount;
     "dfn", dfn;
     "wtffind", wtffind]
    
