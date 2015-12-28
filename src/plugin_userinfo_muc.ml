(*
 * (c) 2004-2010 Anastasia Gornostaeva
 *)

open JID
open Muc

let status muc_context xmpp kind jid_from text =
  if is_joined muc_context jid_from then
    let entity =
      if text = "" then jid_from.lresource
      else Stringprep.resourceprep text
    in
      try
        let item = Nicks.find entity (get_room_env from).nicks in
          make_msg out xml ((if item.status = "" then ""
                             else item.status ^ " ") ^
                              "[" ^ (match item.show with
                                       | `Online -> "online"
                                       | `Away -> "away"
                                       | `DND -> "dnd"
                                       | `Chat -> "free for chat"
                                       | `XA -> "xa") ^ "]")
      with _ ->
        make_msg out xml 
          (Lang.get_msg env.env_lang "plugin_userinfo_status_whose" [])

let plugin opts =
  Muc.add_for_muc_context
    (fun muc_context xmpp ->
       Plugin_command.add_commands xmpp [("status", status)] opts
    )

let () =
  Plugin.add_plugin plugin
