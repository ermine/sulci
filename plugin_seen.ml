(*
 * (c) 2004-2012 Anastasia Gornostaeva
 *)

open JID
open Hooks
open Muc
open XMPPClient

open Sqlite3
module Sql = Seen_sql.Make(Sqlgg_sqlite3)
  
exception Break

type seen_context = {
  db : Sqlite3.db;
  changes : (string, string) Hashtbl.t
}
  
let cmd_greet =
  Pcre.regexp ~flags:[`DOTALL; `UTF8] "([^\\s]+)\\s+([^\\s]+)\\s+(.+)"
    
let add_greet db xmpp env kind jid_from text =
  if text <> "" then
    try
      let res = Pcre.exec ~rex:cmd_greet ~pos:0 text in
      let jid = JID.of_string (Pcre.get_substring res 1) in
      let jid = string_of_jid (bare_jid jid) in
      let room = JID.of_string (Pcre.get_substring res 2) in
      let room = string_of_jid (bare_jid room) in
      let greet = Pcre.get_substring res 3 in
        match Sql.check_greet db ~jid ~room with
          | Some _ ->
              ignore (Sql.update_greet db ~msg:greet ~jid ~room);
              env.env_message xmpp kind jid_from
                (Lang.get_msg env.env_lang "plugin_seen_greet_updated" [])
          | None ->
              ignore (Sql.add_greet db ~jid ~room ~msg:greet);
              env.env_message xmpp kind jid_from
                (Lang.get_msg env.env_lang "plugin_seen_greet_added" [])
    with Not_found ->
      env.env_message xmpp kind jid_from
        (Lang.get_msg env.env_lang "plugin_seen_greet_bad_syntax" [])
    
let muc_seen ctx muc_context xmpp env jid_from event =
  match event with
    | MUC_join -> (
        let nick = jid_from.lresource in
        let room_env = get_room_env muc_context jid_from in
        let item = Occupant.find nick room_env.occupants in
        let room = string_of_jid (bare_jid jid_from) in
        let jid =
          match item.jid with
            | None -> ""
            | Some j -> string_of_jid (bare_jid j)
        in
          match Sql.get_greet ctx.db ~jid ~room with
            | None -> ()
            | Some msg ->
                env.env_message xmpp (Some Groupchat) jid_from
                  (Printf.sprintf "[%s] %s" jid_from.resource msg)
      )
    | MUC_nick (newnick, _reason) ->
        let oldnick = jid_from.lresource in
          Hashtbl.add ctx.changes oldnick newnick
    | MUC_leave reason
    | MUC_ban reason
    | MUC_kick reason
    | MUC_members_only reason -> (
        let nick = jid_from.lresource in
        let room_env = get_room_env muc_context jid_from in
        let item = Occupant.find nick room_env.occupants in
        let room = string_of_jid (bare_jid jid_from) in
        let last = Int64.of_float (Unix.gettimeofday ()) in
        let action =
          match event with
            | MUC_leave _ -> "left"
            | MUC_kick _ -> "kick"
            | MUC_ban _ -> "ban"
            | MUC_members_only _ -> "unmember"
            | _ -> "left"
        in
        let reason =
          match reason with
            | None -> ""
            | Some v -> v
        in
          match item.jid with
            | None -> (
                let jid = "" in
                  match Sql.check_user_by_nick ctx.db ~nick ~room with
                    | None ->
                        ignore (Sql.add_user_by_nick ctx.db ~nick ~jid ~room
                                  ~last ~reason ~action)
                    | Some _ ->
                        ignore (Sql.update_user_by_nick ctx.db ~nick ~room
                                  ~last ~reason ~action)
              )
            | Some j -> (
                let jid = string_of_jid j in
                  match Sql.check_user_by_jid ctx.db ~jid ~room with
                    | None ->
                        ignore (Sql.add_user_by_jid ctx.db ~nick ~jid ~room
                                  ~last ~reason ~action)
                    | Some _ ->
                        ignore (Sql.update_user_by_jid ctx.db ~jid ~room
                                  ~last ~reason ~action)
              )
      )
    | _ ->
        ()
        
let find_nick (jid:string) occupants =
  let result = ref [] in
    Occupant.iter (fun nick item ->
                     match item.jid with
                       | None -> ()
                       | Some j ->
                           if jid = string_of_jid (bare_jid j) then
                             result := nick :: !result
                  ) occupants;
    if !result = [] then raise Not_found else !result
      
let verify_nick nick jid occupants env =
  try
    let item = Occupant.find nick occupants in
      match item.jid with
        | None ->
            Lang.get_msg env.env_lang "plugin_seen_is_here" [nick]
        | Some j ->
            if jid = string_of_jid (bare_jid j) then
              Lang.get_msg env.env_lang "plugin_seen_is_here" [nick]
            else if jid = "" then
              Lang.get_msg env.env_lang "plugin_seen_is_here" [nick]
            else
              try let changed = find_nick jid occupants in
                Lang.get_msg env.env_lang "plugin_seen_changed_nick" 
                  [nick; (String.concat ", " changed)]
              with Not_found ->
                Lang.get_msg env.env_lang "plugin_seen_is_not_same" [nick; nick]
  with Not_found ->
    if jid <> "" then
      let changed = find_nick jid occupants in
        Lang.get_msg env.env_lang "plugin_seen_changed_nick" 
          [nick; (String.concat ", " changed)]
    else
      raise Not_found
        
let seen ctx muc_context xmpp env kind jid_from text =
  if text = "" then
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_seen_whom" [])
  else
    if text = jid_from.resource then
      env.env_message xmpp kind jid_from
        (Lang.get_msg env.env_lang "plugin_seen_you" [])
    else
      let room_env = get_room_env muc_context jid_from in
      let room = string_of_jid (bare_jid jid_from) in
      let reply =
        match Sql.seen_by_nick ctx.db ~nick:text ~room with
          | Some (jid, last, action, reason) -> (
              try verify_nick text jid room_env.occupants env
              with Not_found ->
                let stamp = Int64.to_float last in
                let diff = 
                  Lang.expand_time ~lang:env.env_lang "seen"
                    (int_of_float 
                       (Unix.gettimeofday () -. stamp)) in
                  if reason = "" then
                    Lang.get_msg env.env_lang 
                      (match action with
                         | "kick" -> "plugin_seen_kicked"
                         | "ban" -> "plugin_seen_banned"
                         | "unmember" -> "plugin_seen_unmembered"
                         | _ -> "plugin_seen_left")
                      [text; diff]
                  else
                    Lang.get_msg env.env_lang 
                      (match action with
                         | "kick" -> 
                             "plugin_seen_kicked_reason"
                         | "ban" -> 
                             "plugin_seen_banned_reason"
                         | "unmember" ->
                             "plugin_seen_unmembered_reason"
                         | _ -> 
                             "plugin_seen_left_reason")
                      [text; diff; reason]
            )
          | None -> (
              if Occupant.mem text room_env.occupants then
                Lang.get_msg env.env_lang "plugin_seen_is_here" [text]
              else
                let changed =
                  try Some (Hashtbl.find ctx.changes text)
                  with Not_found -> None in
                  match changed with
                    | Some newnick ->
                        if newnick = jid_from.lresource then
                          Lang.get_msg env.env_lang "plugin_seen_you" []
                        else
                          Lang.get_msg env.env_lang "plugin_seen_changed_nick"
                            [text; newnick]
                    | None ->
                        Lang.get_msg env.env_lang "plugin_seen_never_seen" [text]
            )
      in
        env.env_message xmpp kind jid_from reply
            
let plugin opts =
  let file =
    try List.assoc "file" (List.assoc "db" opts)
    with Not_found -> "sulci_users.db" in
    Muc.add_for_muc_context
      (fun muc_context xmpp ->
         (* get spool path *)
         let db = db_open file in
           ignore (Sql.create_greeting db);
           ignore (Sql.create_index_gr_index db);
           ignore (Sql.create_users db);
           ignore (Sql.create_index_users_index db);
           ignore (Sql.create_index_users_nicks db);
           let ctx = {
             db = db;
             changes = Hashtbl.create 10
           } in
             Muc.add_muc_event_handler muc_context (muc_seen ctx);
             Plugin_command.add_commands xmpp
               ["greet", add_greet db;
                "seen", seen ctx muc_context] opts
      )
  
let () =
  Plugin.add_plugin "seen" plugin
