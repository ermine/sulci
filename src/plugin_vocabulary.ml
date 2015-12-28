(*
 * (c) 2004-2012  Anastasia Gornostaeva
*)

open JID
open Common
open Hooks
open Plugin_command
open XMPPClient

open Sqlite3
module Sql = Wtf_sql.Make(Sqlgg_sqlite3)

type wtf_context = {
  mutable total : int;
  db : Sqlite3.db
}

let storage : (string, wtf_context) Hashtbl.t = Hashtbl.create 5

let get_context xmpp =
  Hashtbl.find storage xmpp.user_data.skey

let dfn_re = Pcre.regexp ~flags:[`DOTALL; `UTF8] "([^=]+)\\s*=\\s*(.*)"

let dfn xmpp env kind jid_from text =
  let key, value =
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
      let ctx = get_context xmpp in
      let r = Sql.dfn_check_by_jid ctx.db
        ~key ~luser:jid_from.lnode ~lserver:jid_from.ldomain in
        match r with
          | Some v ->
            if value = v then
              env.env_message xmpp kind jid_from
                (Lang.get_msg env.env_lang "plugin_vocabulary_dfn_again" [])
            else if value = "" then (
              ignore (Sql.dfn_delete_by_jid ctx.db
                        ~key ~luser:jid_from.lnode ~lserver:jid_from.ldomain);
              ctx.total <- ctx.total - 1;
              env.env_message xmpp kind jid_from
                (Lang.get_msg env.env_lang "plugin_vocabulary_removed" [])
            )
            else
              let nick = jid_from.lnode in
              let stamp = Int64.of_float  (Unix.gettimeofday ()) in
                ignore (Sql.dfn_update_by_jid ctx.db ~stamp ~nick ~key ~value
                          ~luser:jid_from.lnode ~lserver:jid_from.ldomain);
                env.env_message xmpp kind jid_from
                  (Lang.get_msg env.env_lang "plugin_vocabulary_replaced" [])
          | None ->
            if value <> "" then
              let nick = jid_from.lnode in
              let stamp = Int64.of_float (Unix.gettimeofday ()) in
                ignore (Sql.dfn_new ctx.db ~stamp ~key ~value ~nick 
                          ~luser:jid_from.lnode ~lserver:jid_from.ldomain);
                ctx.total <- ctx.total + 1;
                env.env_message xmpp kind jid_from
                  (Lang.get_msg env.env_lang "plugin_vocabulary_recorded" [])
            else
              env.env_message xmpp kind jid_from
                (Lang.get_msg env.env_lang
                   "plugin_vocabulary_nothing_to_remove" [])
                
let wtf xmpp env kind jid_from text =
  if text = "" then
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_vocabulary_invalid_syntax" [])
  else
    let key =
      try
        let q = String.index text '?' in
          String.sub text 0 q
      with Not_found -> text in
    let ctx = get_context xmpp in
      match Sql.get_wtf_one ctx.db ~key with
        | None ->
          env.env_message xmpp kind jid_from
            (Lang.get_msg env.env_lang "plugin_vocabulary_not_found" [])
        | Some (nick, value) ->
          env.env_message xmpp kind jid_from
            (Lang.get_msg env.env_lang "plugin_vocabulary_answer"
               [nick; key; value])
              
let collect_records env nick key value (i, acc) =
  let str = Printf.sprintf "%d) %s"
    i
    (Lang.get_msg env.env_lang "plugin_vocabulary_answer"
       [nick; key; value])
  in
    (succ i, str :: acc)
      
let wtfall xmpp env kind jid_from text =
  if text = "" then
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_vocabulary_invalid_syntax" [])
  else
    let key =
      try
        let q = String.index text '?' in
          String.sub text 0 q
      with Not_found -> text
    in
    let ctx = get_context xmpp in
    let (_, reply) =
      Sql.Fold.get_wtf_all ctx.db ~key (collect_records env) (1, []) in
      if reply = [] then
        env.env_message xmpp kind jid_from
          (Lang.get_msg env.env_lang "plugin_vocabulary_not_found" [])
      else
        env.env_message xmpp kind jid_from (String.concat "\n" (List.rev reply))
          
let wtfrand xmpp env kind jid_from text =
  let ctx = get_context xmpp in
  let key = trim (text) in
    if key = "" then
      let reply =
        if ctx.total > 0 then
          let rand = Int64.of_int (Random.int (ctx.total)) in
            match Sql.get_rand ctx.db ~rand with
              | None ->
                Lang.get_msg env.env_lang "plugin_vocabulary_db_is_empty" []
              | Some (nick, key, value) ->
                Lang.get_msg env.env_lang "plugin_vocabulary_answer"
                  [nick; key; value]
        else
          Lang.get_msg env.env_lang "plugin_vocabulary_db_is_empty" []
      in
        env.env_message xmpp kind jid_from reply
    else
      match Sql.get_key_total ctx.db ~key with
        | Some r -> (
          if r > 0L then
            let rand = Int64.of_int (Random.int (Int64.to_int r)) in
              match Sql.get_rand_key ctx.db ~key ~rand with
                | Some (nick, value) ->
                  env.env_message xmpp kind jid_from
                    (Lang.get_msg env.env_lang "plugin_vocabulary_answer"
                       [nick; key; value])
                | None ->
                  env.env_message xmpp kind jid_from
                    (Lang.get_msg env.env_lang
                       "plugin_vocabulary_not_found" [])
          else
            env.env_message xmpp kind jid_from
              (Lang.get_msg env.env_lang "plugin_vocabulary_not_found" [])
        )
        | None ->
          env.env_message xmpp kind jid_from
            (Lang.get_msg env.env_lang "plugin_vocabulary_not_found" [])
                
let wtfcount xmpp env kind jid_from text =
  let ctx = get_context xmpp in
  let key = trim(text) in
  let count =
    if key = "" then
      ctx.total
    else
      match Sql.wtf_count ctx.db ~key with
        | None -> 0
        | Some i -> Int64.to_int i
  in
    if count > 0 then
      env.env_message xmpp kind jid_from
        (Lang.get_msg env.env_lang "plugin_vocabulary_records"
           [string_of_int count])
    else
      env.env_message xmpp kind jid_from
        (Lang.get_msg env.env_lang
           "plugin_vocabulary_db_is_empty" [])
        
let wtffind xmpp env kind jid_from text =
  if text = "" then
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_vocabulary_invalid_syntax" [])
  else
    let ctx = get_context xmpp in
    let (_, reply) = Sql.Fold.wtffind ctx.db ~key:text ~value:text
      (collect_records env) (1, []) in
      if reply = [] then
        env.env_message xmpp kind jid_from
          (Lang.get_msg env.env_lang "plugin_vocabulary_not_found" [])
      else
        env.env_message xmpp kind jid_from (String.concat "\n" (List.rev reply))
          
let wtfremove_re = Pcre.regexp ~flags:[`DOTALL; `UTF8] "([^=]+)(\\s*=\\s*(.*))?"
  
let wtfremove xmpp env kind jid_from text =
  let key, value =
    try
      let res = Pcre.exec ~rex:wtfremove_re text in
      let key = Pcre.get_substring res 1 in
      let value = try Pcre.get_substring res 3 with Not_found -> "" in
        key, value
    with Not_found ->
      ("", "")
  in
    if key = "" then
      env.env_message xmpp kind jid_from
        (Lang.get_msg env.env_lang "plugin_vocabulary_invalid_syntax" [])
    else
      let ctx = get_context xmpp in
      let records =
        if value = "" then
          Sql.delete_key ctx.db ~key
        else
          Sql.delete_key_value ctx.db ~key ~value
      in
        env.env_message xmpp kind jid_from
          (Lang.get_msg env.env_lang "plugin_vocabulary_removed"
             [string_of_int (Int64.to_int records)])
          
let plugin opts =
  let file =
    try List.assoc "file" (List.assoc "db" opts)
    with Not_found ->
      raise
        (Plugin.Error
           "Please specify <db file='/path/wtf.db'/> element in configuration file"
        ) in
    add_for_token
      (fun _opts user_data ->
        let db = db_open file in
          ignore (Sql.create_wtf db);
          ignore (Sql.create_index_dfnidx db);
          ignore (Sql.create_index_dfncheck db);
          let t =
            match Sql.total db with
              | None -> 0
              | Some i -> Int64.to_int i
          in
          let ctx = {
            total = t;
            db = db
          } in
            Hashtbl.add storage user_data.skey ctx;
            add_commands user_data [("dfn", dfn);
                               ("wtf", wtf);
                               ("wtfall", wtfall);
                               ("wtfrand", wtfrand);
                               ("wtfcount", wtfcount);
                               ("wtffind", wtffind);
                               ("wtfremove", wtfremove)
                              ] opts
      )
      
let () =
  Plugin.add_plugin "vocabulary" plugin
    
