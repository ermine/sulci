(*
 * (c) 2004-2010  Anastasia Gornostaeva
*)

open XMPP
open JID
open Muc
open Hooks
open Plugin_vocabulary

let dfn muc_context xmpp env kind jid_from text =
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
      let room_env = get_room_env muc_context jid_from in
      let nick = jid_from.lresource in
      let item = Occupant.find nick room_env.occupants in
      let r =
        match item.jid with
          | None ->
              Sql.dfn_check_by_occupant ctx.db
                ~key ~nick ~luser:jid_from.lnode ~lserver:jid_from.ldomain
          | Some jid ->
              Sql.dfn_check_by_jid ctx.db
                ~key ~luser:jid.lnode ~lserver:jid.ldomain
      in
        match r with
          | Some v ->
              if value = v then
                env.env_message xmpp kind jid_from
                  (Lang.get_msg env.env_lang "plugin_vocabulary_dfn_again" [])
              else if value = "" then (
                (match item.jid with
                   | None ->
                       ignore (Sql.dfn_delete_by_occupant ctx.db ~key ~nick
                                 ~luser:jid_from.lnode
                                 ~lserver:jid_from.ldomain)
                   | Some jid ->
                       ignore (Sql.dfn_delete_by_jid ctx.db ~key
                                 ~luser:jid.lnode ~lserver:jid.ldomain)
                );
                ctx.total <- ctx.total - 1;
                env.env_message xmpp kind jid_from
                  (Lang.get_msg env.env_lang "plugin_vocabulary_removed" [])
              )
              else
                let nick = jid_from.lresource in
                let stamp = Int64.of_float  (Unix.gettimeofday ()) in
                  
                  (match item.jid with
                     | None ->
                         ignore (Sql.dfn_update_by_occupant ctx.db ~stamp ~nick
                                   ~key ~value ~luser:jid_from.lnode
                                   ~lserver:jid_from.ldomain)
                     | Some jid ->
                         ignore (Sql.dfn_update_by_jid ctx.db ~stamp ~nick ~key
                                   ~value ~luser:jid.lnode ~lserver:jid.ldomain);
                  );
                  env.env_message xmpp kind jid_from
                    (Lang.get_msg env.env_lang "plugin_vocabulary_replaced" [])
          | None ->
              if value <> "" then
                let nick = jid_from.lresource in
                let stamp = Int64.of_float (Unix.gettimeofday ()) in
                  (match item.jid with
                     | None ->
                         ignore (Sql.dfn_new ctx.db ~stamp ~key ~value
                                   ~nick ~luser:jid_from.lnode
                                   ~lserver:jid_from.ldomain)
                     | Some jid ->
                         ignore (Sql.dfn_new ctx.db ~stamp ~key ~value
                                   ~nick ~luser:jid.lnode ~lserver:jid.ldomain)
                  );
                  ctx.total <- ctx.total + 1;
                  env.env_message xmpp kind jid_from
                    (Lang.get_msg env.env_lang "plugin_vocabulary_recorded" [])
              else
                env.env_message xmpp kind jid_from
                  (Lang.get_msg env.env_lang
                     "plugin_vocabulary_nothing_to_remove" [])

let plugin opts =
  Muc.add_for_muc_context
    (fun muc_context xmpp ->
       Plugin_command.add_commands xmpp ["dfn", (dfn muc_context)] opts
    )

let () =
  Plugin.add_plugin "vocabulary_muc" plugin
