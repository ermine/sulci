(*
 * (c) 2004-2012 Anastasia Gornostaeva
 *)

open JID
open Hooks
open Muc
open XMPPClient

let msg muc_context xmpp env kind jid_from text =
  try
    let s = String.index text ' ' in
    let jid_to = JID.of_string (String.sub text 0 s) in
    let body = Common.string_after text (s+1) in
      if is_joined muc_context jid_to && is_bare jid_to then
        env.env_message xmpp (Some Groupchat) jid_to body
      else
        env.env_message xmpp (Some Groupchat) jid_to body;
      env.env_message xmpp kind jid_from "done"
  with _ ->
    env.env_message xmpp kind jid_from "?"

let join_rex = Pcre.regexp "([^\\s]+)(\\s+(.*))?"

let join muc_context xmpp env kind jid_from text =
  try
    let r = Pcre.exec ~rex:join_rex text in
    let room = JID.of_string (Pcre.get_substring r 1) in
      if is_joined muc_context room then
        env.env_message xmpp kind jid_from "again?"
      else        
        let nick = try Some (Pcre.get_substring r 3) with Not_found -> None in
(*          
        let lang = Lang.deflang in
        let chatlog = true in
        let filter = "cerberus" in
          Muc.register_room ~lang ~filter nick room;
          Muc.add_room room nick lang chatlog filter;
          out (Muc.join_room nick (room.lnode, room.ldomain))
*)
          Muc.enter_room muc_context xmpp ~maxstanzas:0 ?nick room;
          env.env_message xmpp kind jid_from "done"
  with _ ->
    env.env_message xmpp kind jid_from "try again"
      
let leave_rex = Pcre.regexp "([^\\s]+)(\\s+(.*))?"
  
let leave muc_context xmpp env kind jid_from text =
  try
    let r = Pcre.exec ~rex:leave_rex text in
    let room_s = Pcre.get_substring r 1
    and reason = try Some (Pcre.get_substring r 3) with Not_found -> None in
    let room = JID.of_string room_s in
      if is_joined muc_context room then (
        Muc.leave_room muc_context xmpp ?reason room;
        if not (jid_from.lnode = room.lnode &&
            jid_from.ldomain = room.ldomain) then
          env.env_message xmpp kind jid_from "done";
      )
      else
        env.env_message xmpp kind jid_from "i haven't entered to that room"
  with exn ->
    env.env_message xmpp kind jid_from "hmm?"
        
let invite_rex = Pcre.regexp "([^\\s]+)(\\s+(.*))?"
  
let invite muc_context xmpp env kind jid_from text =
  try
    let r = Pcre.exec ~rex:leave_rex text in
    let who = Pcre.get_substring r 1
    and room_dst = try Pcre.get_substring r 3 with Not_found -> "" in
      if room_dst = "" && is_joined muc_context jid_from then
        let jid_who = JID.of_string who in
          Muc.invite xmpp jid_from jid_who
      else
        let room_jid = JID.of_string room_dst in
          if room_jid.lresource = "" && is_joined muc_context room_jid then
            let room_env = get_room_env muc_context jid_from in
              try
                match (Occupant.find who room_env.occupants).jid with
                  | Some jid_who ->
                      Muc.invite xmpp room_jid jid_who
                  | None ->
                      raise Not_found
              with Not_found ->
                let jid_who = JID.of_string who in
                  Muc.invite xmpp room_jid jid_who
          else
            raise Not_found
  with exn ->
    env.env_message xmpp kind jid_from "hmm?"
  
let change_nick muc_context xmpp env kind jid_from text =
  if text = "" then
    env.env_message xmpp kind jid_from "which nick?"
  else
    if is_joined muc_context jid_from then
      Muc.change_nick xmpp jid_from text
    else
      env.env_message xmpp kind jid_from "it is not a room"
    
let plugin opts =
  Muc.add_for_muc_context
    (fun muc_context xmpp ->
       Plugin_command.add_commands xmpp
         [("msg", msg muc_context);
          ("join", join muc_context);
          ("leave", leave muc_context);
          ("invite", invite muc_context);
          ("nick", change_nick muc_context)
         ] opts
    )
  
let () =
  Plugin.add_plugin "admin_muc" plugin
