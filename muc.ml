(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open XMPP
open Jid
open Xep_muc
open Common
open Hooks

type t = {
  max_public_message_length : int
}

let global = {
  max_public_message_length = 400
}
    
(*
open Sqlite3
open Sqlite_util

let file =
  try trim (Light_xml.get_cdata Config.config ~path:["muc"; "db"])
  with Not_found -> "sulci_muc.db"

let table = "muc"

let db =
  let db = Sqlite3.db_open file in
    create_table file db
      (Printf.sprintf
         "SELECT name FROM SQLITE_MASTER WHERE type='table' AND name='%s'" table)
      (Printf.sprintf
         "CREATE TABLE %s (room text, nick text, lang text, chatlog char(1), filter text)"
         table);
    db

let add_room room nick lang chatlog filter =
  let sql = Printf.sprintf
    "INSERT INTO %s (room, nick, lang, chatlog, filter) VALUES(%s, %s, %s, '%s', %s)"
    table (escape (string_of_jid room)) (escape nick) (escape lang)
    (if chatlog then "T" else "F") (escape filter) in
    simple_exec file db sql
      
let load_rooms xmpp =
  let sql = Printf.sprintf "SELECT room, nick, lang, chatlog, filter FROM %s"
    table in
  let rec iter_room stmt =
    match get_row stmt with
      | Some row ->
          let room = Jid.jid_of_string (Data.to_string row.(0)) in
          let mynick = Data.to_string row.(1) in
          let lang = Data.to_string row.(2) in
          let chatlog =
            match Data.to_string row.(3) with
              | "T" -> true
              | _ -> false
          in
          let filter = Data.to_string row.(4) in
            (*
            if chatlog then
              Muc_log.add_chatlog room;
             *)
            register_room ~lang ~filter mynick room;
            out (join_room mynick (room.lnode, room.ldomain)); 
            iter_room stmt
      | None ->
          ()
  in
    try
      let stmt = prepare db sql in
        iter_room stmt
    with Sqlite3.Error _ ->
      exit_with_rc file db sql

*)

type occupant = {
  (* nick : string; *)
  mutable jid : Jid.jid option;
  mutable affiliation : affiliation;
  mutable role : role
}

module OccupantNick =
struct
  type t = string
  let compare x y = if x = y then 0 else if x < y then 1 else -1
end
module Occupant = Map.Make(OccupantNick)

type room_env = {
  mutable mynick : string;
  lang : string;
  mutable occupants : occupant Occupant.t
}

module GroupID =
struct
  type t = string * string
  let compare = compare
end
module Groupchat = Map.Make(GroupID)
let groupchats = ref Groupchat.empty
    
let get_room_env jid =
  Groupchat.find (jid.lnode, jid.ldomain) !groupchats

let get_entity text from =
  if text = "" then
    EntityYou from
  else
    let room_env = get_room_env from in
      if room_env.mynick = text then
        EntityMe from
      else if Occupant.mem text room_env.occupants then
        if from.resource = text then
          EntityYou from
        else
          EntityUser (text, {from with resource = text; lresource = text})
      else
        let jid = try jid_of_string text with _ -> raise BadEntity in
          if Jid.equal jid from then
            EntityYou jid
          else if jid.lnode = "" then (
            (try dnsprep jid.ldomain
             with _ -> raise BadEntity);
            EntityHost jid
          ) else
            EntityUser (text, jid)

let check_access jid classname =
  match catch get_room_env jid with
    | None -> Hooks.check_access jid classname
    | Some room_env ->
        match catch (Occupant.find jid.lresource) room_env.occupants with
          | None -> Hooks.check_access jid classname
          | Some occupant ->
              match occupant.jid with
                | None -> Hooks.check_access jid classname
                | Some ojid -> Hooks.check_access ojid classname

let nick_sep = [':'; ','; '.'; '>']

exception NickBody of string * string
  
let split_nick_body room_env body =
  try
    let len_body = String.length body in
      Occupant.iter (fun nick _ ->
                          if is_prefix nick body then
                            let len_nick = String.length nick in
                              if len_nick = len_body then
                                raise (NickBody (nick, ""))
                              else if List.mem body.[len_nick] nick_sep then
                                raise (NickBody
                                         (nick, skip_ws body (len_nick + 2)))
                              else
                                ()
                          else
                            ()
                       ) room_env.occupants;
      "", body
  with
    | NickBody (nick, body) ->
        nick, body

type reason = string
type password = string
    
type muc_event =
  | MUC_leave of reason option
  | MUC_nick of string * reason option
  | MUC_destroy of jid option * password option * reason option
  | MUC_ban of reason option
  | MUC_kick of reason option
  | MUC_affiliation of reason option
  | MUC_members_only of reason option
  | MUC_system_shutdown of reason option
  | MUC_decline of jid option * jid option * reason option
  | MUC_invite of jid option * jid option * reason option * password option

let hook_muc_event xmpp env jid_from event =
  ()

let make_msg xmpp kind jid_from ?response_tail response =
  match kind with
    | Some Groupchat ->
        let tail =
          match response_tail with
            | None -> ""
            | Some t -> "\n" ^ t
        in
        let limit = 
          let l = global.max_public_message_length - String.length tail in
            if l < 0 then 0 else l in
        let resp = sub_utf8_string response limit in
        let cut, respo =
          if String.length resp < String.length response then
            true, clean_tail resp ^ "[...]" ^ tail
          else 
            false, resp ^ tail
        in
        let body =
          if Pcre.pmatch ~pat:"/me" response then respo else
            jid_from.resource ^ ": " ^ respo
        in
          send_message xmpp ~jid_to:(bare_jid jid_from) ?kind ~body ();
          if cut then
            let msgs =
              split_long_message Hooks.global.max_message_length response tail in
              List.iter (fun body ->
                           send_message xmpp ~kind:Chat ~jid_to:jid_from ~body ()
                        ) msgs
    | _ ->
        Hooks.make_msg xmpp kind jid_from ?response_tail response

let get_reason = function
  | None -> None
  | Some i -> i.User.reason
  
let process_presence_user xmpp env stanza from room_env data enter =
  let () =
    match data.User.item with
      | None -> ()
      | Some item ->
          if stanza.kind = None then
            let occupant =
              Occupant.find from.lresource room_env.occupants in
              (match item.User.jid with
                 | None -> ()
                 | Some jid -> occupant.jid <- Some jid);
              (match item.User.role with
                 | None -> ()
                 | Some r -> occupant.role <- r);
              (match item.User.affiliation with
                 | None -> ()
                 | Some a -> occupant.affiliation <- a)
  in
  let removal =
    match data.User.destroy with
      | None -> false
      | Some (venue, reason) ->
          if stanza.kind = Some Unavailable then (
            hook_muc_event xmpp env from
              (MUC_destroy (venue, data.User.password, reason));
            true
          ) else
            false
  in
  let removal = List.fold_left
    (fun removal -> function
       | 100 ->
           (* context: Entering a room *)
           (* Inform user that any occupant is allowed to see the user's
              full JID *)
           removal
       | 110 ->
           (* context Any room presence *)
           (* Inform user that presence refers to one of its own
              room occupants *)
           removal
       | 170 ->
           (* context Configuration change *)
           (* Inform occupants that room logging is now enabled *)
           removal
       | 201 ->
           (* context Entering a room *)
           (* Inform user that a new room has been created *)
           removal
       | 210 ->
           (* context Entering a room *)
           (* Inform user that service has assigned or modified
              occupant's roomnick *)
           if enter then
             room_env.mynick <- from.lresource;
           removal
       | 301 ->
           (* context Removal from room *)
           (* Inform user that he or she has been banned from the room *)
           if stanza.kind = Some Unavailable then
             let reason = get_reason data.User.item in
               hook_muc_event xmpp env from (MUC_ban reason);
               true
           else
             removal
       | 303 -> (
           (* context Exiting a room *)
           (* Inform all occupants of new room nickname *)
           if stanza.kind = None then
             match data.User.item with
               | None -> removal
               | Some i ->
                   match i.User.nick with
                     | None -> removal
                     | Some newnick ->
                         if from.lresource = room_env.mynick then
                           room_env.mynick <- newnick;
                         room_env.occupants <- Occupant.add newnick
                           (Occupant.find from.lresource room_env.occupants)
                           room_env.occupants;
                         let reason = get_reason data.User.item in
                           hook_muc_event xmpp env from
                             (MUC_nick (newnick, reason));
                           true
           else
             removal
         )
       | 307 -> (
           (* context Removal from room *)
           (* Inform user that he or she has been kicked from the room *)
           if stanza.kind = Some Unavailable then
             let reason = get_reason data.User.item in
               hook_muc_event xmpp env from (MUC_kick reason);
               true
           else
             removal
           )
       | 321 -> (
           (* context Removal from room *)
           (* Inform user that he or she is being removed from
              the room because of an affiliation change *)
           if stanza.kind = Some Unavailable then
             let reason = get_reason data.User.item in
               hook_muc_event xmpp env from (MUC_affiliation reason);
               true
           else
             removal
         )
       | 322 -> (
           (* context Removal from room *)
           (* Inform user that he or she is being removed from the room
              because the room has been changed to
              members-only and the user is not a member *)
           if stanza.kind = Some Unavailable then
             let reason = get_reason data.User.item in
               hook_muc_event xmpp env from (MUC_members_only reason);
               true
           else
             removal
         )
       | 332 ->
           (* context Removal from room *)
           (* Inform user that he or she is being removed from the room
              because of a system shutdown *)
           if stanza.kind = Some Unavailable then
             let reason = get_reason data.User.item in
               hook_muc_event xmpp env from (MUC_system_shutdown reason);
               true
           else
             removal
       | _ ->
           removal
    ) removal data.User.status in
    if stanza.kind = Some Unavailable && not removal then
      hook_muc_event xmpp env from (MUC_leave stanza.content.status)

let process_presence_x xmpp env stanza from room_env enter =
  List.iter (function
               | Xmlelement (qname, _, _) as el ->
                   if qname = (ns_muc_user, "x") then
                     process_presence_user xmpp env stanza from room_env
                       (User.decode el) enter
               | _ ->
                   ()
            ) stanza.x
                          
let process_presence xmpp env stanza hooks =
  match stanza.jid_from with
    | None -> do_hook xmpp env stanza hooks
    | Some from ->
        match catch get_room_env from with
          | None -> do_hook xmpp env stanza hooks
          | Some room_env ->
              let env = {env_groupchat = true;
                         env_lang = room_env.lang;
                         env_get_entity = get_entity;
                         env_message = make_msg;
                        }
              in
                match stanza.kind with
                  | None
                  | Some Unavailable ->
                      let enter =
                        if stanza.kind = None &&
                          not (Occupant.mem from.lresource
                                 room_env.occupants) then
                            true
                        else
                          false
                      in
                        if enter then
                          room_env.occupants <- Occupant.add from.lresource
                            {jid = None;
                             affiliation = AffiliationNone;
                             role = RoleNone} room_env.occupants;
                        process_presence_x xmpp env stanza from
                          room_env enter;
                        if stanza.kind = Some Unavailable then
                          if from.lresource = room_env.mynick then
                            groupchats :=
                              Groupchat.remove (from.lnode, from.ldomain)
                                !groupchats
                          else
                            room_env.occupants <-
                              Occupant.remove from.lresource room_env.occupants;
                        do_hook xmpp env stanza hooks
                  | _ ->
                      do_hook xmpp env stanza hooks

let process_invite xmpp env jid_from jid_to reason password () =
  ()

let process_decline xmpp env jid_from jid_to reason () =
  ()

let process_message_status xmpp env stanza status =
  List.iter 
    (function
       | 100
           (* context: Entering a room *)
           (* Inform user that any occupant is allowed to see the user's
              full JID *)
       | 101
           (* message out of band *)
           (* context Affiliation change *)
           (* Inform user that his or her affiliation changed
              while not in the room *)
       | 102
           (* context Configuration change *)
           (* Inform occupants that room now shows unavailable
              members *)
       | 103
           (* context Configuration change *)
           (* Inform occupants that room now does not show
              unavailable members *)
       | 104
           (* context Configuration change *)
           (* Inform occupants that a non-privacy-related room
              configuration change has occured *)
       | 170
           (* context Configuration change *)
           (* Inform occupants that room logging is now enabled *)
       | 171
           (* context Configuration change *)
           (* Inform occupants that room logging is now disabled *)
       | 172
           (* context Configuration change *)
           (* Inform occupants that the room is now non- anonymous *)
       | 173
           (* context Configuration change *)
           (* Inform occupants that the room is now semi- anonymous *)
       | 174 ->
           (* context Configuration change *)
           (* Inform occupants that the room is now fully-anonymous *)
           ()
       | _ ->
           ()
    ) status
             
let process_message_user xmpp env stanza from data =
  let () =
    match stanza.kind with
      | None
      | Some Normal -> (
          match data.User.decline with
            | None -> ()
            | Some (jid_from, jid_to, reason) ->
                hook_muc_event xmpp env from
                  (MUC_decline (jid_from, jid_to, reason))
        );
          List.iter
            (fun (jid_from, jid_to, reason) ->
               hook_muc_event xmpp env from
                 (MUC_invite (jid_from, jid_to, reason, data.User.password))
            ) data.User.invite
      | Some Groupchat ->
          ()
      | _ ->
          ()
  in
    process_message_status xmpp env stanza data.User.status;
    if data.User.decline <> None || data.User.invite <> [] ||
      data.User.status <> [] then
        true
    else
      false

let process_message xmpp env stanza hooks =
  match stanza.jid_from with
    | None -> do_hook xmpp env stanza hooks
    | Some from ->
        let env =
          try
            let room_env = get_room_env from in
              {env_groupchat = true;
               env_lang = room_env.lang;
               env_get_entity = get_entity;
               env_message = make_msg;
              }
          with Not_found -> env
        in
        let continue =
          match catch (get_element (ns_muc_user, "x")) stanza.x with
            | Some el ->
                process_message_user xmpp env stanza from (User.decode el)
            | None ->
                true
        in
        let continue =
          if continue && from.lresource <> "" then
            match stanza.kind with
              | Some Chat
              | Some Groupchat -> (
                  match catch get_room_env from with
                    | None -> true
                    | Some room_env ->
                        if from.lresource = room_env.mynick then
                          false (* echo *)
                        else
                          match stanza.content.subject, stanza.content.body with
                            | None, Some b ->
                                let nick, text = split_nick_body room_env b in
                                  if nick = "" then
                                    true
                                  else if nick = room_env.mynick then (
                                    do_hook xmpp env
                                      {stanza with content =
                                          {stanza.content with body = Some text}}
                                      hooks;
                                    false
                                  )
                                  else
                                    false
                            | None, None ->
                                true
                            | Some s, _ ->
                                true
                )
              | _ ->
                  true
          else
            true
        in
          if continue then
            do_hook xmpp env stanza hooks

let enter_room xmpp ?maxchars ?maxstanzas ?seconds ?since ?password
    nick room =
  groupchats := Groupchat.add (room.lnode, room.ldomain)
    {mynick = nick;
     lang = xmpp.xmllang;
     occupants = Occupant.empty} !groupchats;
  send_presence xmpp ~jid_to:(replace_resource room nick)
    ~x:[encode_muc ?maxchars ?maxstanzas ?seconds ?since ?password ()] ()

let leave_room xmpp ?status room =
  let mynick = (get_room_env room).mynick in
    send_presence xmpp ~jid_to:(replace_resource room mynick)
      ~kind:Unavailable ?status ()
    
let invite xmpp ?reason jid_room who =
  XMPP.send_message xmpp ~jid_to:(bare_jid jid_room)
    ~x:[User.encode_invite ~jid_to:who ?reason ()] ()

let kick xmpp ?reason jid_room nick callback =
  XMPP.make_iq_request xmpp ~jid_to:(bare_jid jid_room)
    (IQSet (Admin.encode_item ~nick ~role:RoleNone ?reason ())) callback
    
let ban xmpp ?reason jid_room (jid:string) callback =
  XMPP.make_iq_request xmpp ~jid_to:(bare_jid jid_room)
    (IQSet (Admin.encode_item ?reason ~affiliation:AffiliationOutcast ~jid ()))
    callback

let set_topic xmpp jid_room subject =
  XMPP.send_message xmpp ~jid_to:(bare_jid jid_room)
    ~kind:Groupchat ~subject ()
    
    
let plugin opts =
  Hooks.add_message_hook 10 "muc" process_message;
  Hooks.add_presence_hook 10 "muc" process_presence;
  register_on_connect
    (fun xmpp ->
       enter_room xmpp ~maxstanzas:0
         "stoat-2.0" (make_jid "sulci" "conference.jabber.ru" "")
    )

let _ =
  add_plugin "muc" plugin
