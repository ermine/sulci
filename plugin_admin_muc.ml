(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open Jid
open Types
open Common
open Hooks
open Muc_types
open Muc
open Nicks

let join_rex = Pcre.regexp "([^\\s]+)(\\s+(.*))?"
  
let join text from xml env out =
  if env.env_check_access from "admin" then
    try
      let r = Pcre.exec ~rex:join_rex text in
      let room = jid_of_string (Pcre.get_substring r 1) in
        if not (is_groupchat room) then
          let nick = try Pcre.get_substring r 3 with Not_found ->
            trim (get_cdata Config.config ~path:["jabber"; "user"]) in
          let lang = Lang.deflang in
          let chatlog = true in
          let filter = "cerberus" in
            Muc.register_room ~lang ~filter nick room;
            Muc.add_room room nick lang chatlog filter;
            out (Muc.join_room nick (room.lnode, room.ldomain))
        else
          make_msg out xml "again?"
    with _ ->
      ()
  else
    make_msg out xml (Lang.get_msg env.env_lang "plugin_admin_join_no_access" [])
      
let leave_rex = Pcre.regexp "([^\\s]+)(\\s+(.*))?"
  
let leave text from xml env out =
  if env.env_check_access from "admin" then
    try
      let r = Pcre.exec ~rex:leave_rex text in
      let room_s = Pcre.get_substring r 1
      and reason = 
        try Some (Pcre.get_substring r 3) with Not_found -> None in
      let room = jid_of_string room_s in
        if is_groupchat room then
          out (Muc.leave_room (room.lnode, room.ldomain) ?reason)
        else
          raise Not_found
    with exn ->
      make_msg out xml "hmm?"
        
        
let invite_rex = Pcre.regexp "([^\\s]+)(\\s+(.*))?"
  
let invite text from xml env out =
  if env.env_check_access from "admin" then
    try
      let r = Pcre.exec ~rex:leave_rex text in
      let who = Pcre.get_substring r 1
      and room_dst = try Pcre.get_substring r 3 with Not_found -> "" in
        if room_dst = "" && env.env_groupchat then
          let _ = jid_of_string who in
            out (Muc.invite (from.lnode, from.ldomain) who)
        else
          let rjid = jid_of_string room_dst in
            if rjid.lresource = "" && env.env_groupchat then
              let room_env = get_room_env from in
                try
                  match (Nicks.find who room_env.nicks).jid with
                    | Some j ->
                        out (Muc.invite (rjid.lnode, rjid.ldomain)
                               (j.lnode ^ "@" ^ j.ldomain))
                    | None ->
                        raise Not_found
                with Not_found ->
                  let _ = jid_of_string who in
                    out (Muc.invite (rjid.lnode, rjid.ldomain) 
                           who)
            else
              raise Not_found
    with exn ->
      make_msg out xml "hmm?"
        
  
let _ =
  register_command "msg"
    (Plugin_admin.msg
       ~is_groupchat:(fun jid -> Muc.is_groupchat jid && jid.lresource = ""));
  register_command "join" join;
  register_command "leave" leave;
  register_command "invite" invite;
