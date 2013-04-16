(*
 * (c) 2004-2012 Anastasia Gornostaeva
 *)

open Xml
open JID
open Common
open Hooks

open XMPPClient
module MUC = XEP_muc.Make(XMPPClient)
open MUC

open Sqlite3
module Sql = Muc_sql.Make(Sqlgg_sqlite3)

type occupant = {
  (* nick : string; *)
  mutable jid : JID.t option;
  mutable affiliation : affiliation;
  mutable role : role
}

module Occupant = Map.Make(String)

type room_env = {
  mutable mynick : string;
  mutable can_send : bool;
  queue : (string * (xmpp -> unit -> unit)) Queue.t;
  lang : string;
  mutable occupants : occupant Occupant.t
}
  
module GroupID =
struct
  type t = string * string
  let compare = Pervasives.compare
end
module Groupchat = Map.Make(GroupID)

type reason = string
type password = string

type muc_event =
  | MUC_join
  | MUC_leave of reason option
  | MUC_nick of string * reason option
  | MUC_destroy of JID.t option * password option * reason option
  | MUC_ban of reason option
  | MUC_kick of reason option
  | MUC_room_created
  | MUC_affiliation of reason option
  | MUC_members_only of reason option
  | MUC_system_shutdown of reason option
  | MUC_decline of JID.t option * JID.t option * reason option
  | MUC_invite of JID.t option * JID.t option * reason option * password option

type muc_context = {
  max_public_message_length : int;
  default_mynick : string option;
  db : Sqlite3.db;
  mutable groupchats : room_env Groupchat.t;
  mutable conversation_procs :
    (muc_context -> xmpp -> env -> message_type option ->
     JID.t -> string -> string -> unit) list;
  mutable muc_event_handlers :
    (muc_context -> xmpp -> env -> JID.t -> muc_event -> unit) list;
}

let ctx_hooks : (muc_context -> user_data -> unit) list ref = ref []

let add_muc_event_handler muc_context handler =
  muc_context.muc_event_handlers <- handler :: muc_context.muc_event_handlers
    
let hook_muc_event muc_context xmpp env jid_from event =
  List.iter (fun proc -> proc muc_context xmpp env jid_from event)
    muc_context.muc_event_handlers

let add_hook_conversation ctx proc =
  ctx.conversation_procs <- proc :: ctx.conversation_procs
    
let process_conversation nick text ctx xmpp env stanza hooks =
  let () =
    match stanza.jid_from with
      | None -> ()
      | Some from ->
          List.iter (fun proc ->
                       proc ctx xmpp env
                         stanza.content.message_type from nick text)
            ctx.conversation_procs;
  in
    do_hook xmpp env stanza hooks

(*
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
          let room = JID.JID.of_string (Data.to_string row.(0)) in
          let mynick = Data.to_string row.(1) in
          let lang = Data.to_string row.(2) in
          let chatlog =
            match Data.to_string row.(3) with
              | "T" -> true
              | _ -> false
          in
          let filter = Data.to_string row.(4) in
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

let get_room_env ctx jid =
  Groupchat.find (jid.lnode, jid.ldomain) ctx.groupchats

let is_joined ctx jid =
  Groupchat.mem (jid.lnode, jid.ldomain) ctx.groupchats

let get_entity ctx text jid_from =
  if text = "" then
    EntityYou jid_from
  else
    let room_env = get_room_env ctx jid_from in
      if room_env.mynick = text then
        EntityMe jid_from
      else if Occupant.mem text room_env.occupants then
        if jid_from.resource = text then
          EntityYou jid_from
        else
          EntityUser (text, {jid_from with resource = text; lresource = text})
      else
        let jid = try JID.of_string text with _ -> raise BadEntity in
          if JID.equal jid jid_from then
            EntityYou jid
          else if jid.lnode = "" then (
            (try dnsprep jid.ldomain
             with _ -> raise BadEntity);
            EntityHost jid
          ) else
            EntityUser (text, jid)

let check_access xmpp jid classname =
  match opt_try (get_room_env xmpp) jid with
    | None -> Acl.check_access jid classname
    | Some room_env ->
        match opt_try (Occupant.find jid.lresource) room_env.occupants with
          | None -> Acl.check_access jid classname
          | Some occupant ->
              match occupant.jid with
                | None -> Acl.check_access jid classname
                | Some ojid -> Acl.check_access ojid classname

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

let make_msg ctx xmpp kind jid_from ?response_tail response =
  match kind with
    | Some Groupchat ->
        let tail =
          match response_tail with
            | None -> ""
            | Some t -> "\n" ^ t
        in
        let limit =
          let l = ctx.max_public_message_length - String.length tail in
            if l < 0 then 0 else l in
        let resp = sub_utf8_string response limit in
        let cut, respo =
          if String.length resp < String.length response then
            true, clean_tail resp ^ "[...]" ^ tail
          else 
            false, resp ^ tail
        in
        let body =
          if Pcre.pmatch ~pat:"/me" response || jid_from.resource = "" then respo
          else jid_from.resource ^ ": " ^ respo
        in
        let room_env = get_room_env ctx jid_from in
          if room_env.can_send then (
            XMPPClient.send_message xmpp
              ~jid_to:(bare_jid jid_from) ?kind ~body ();
            room_env.can_send <- false
          ) else (
            let send_message xmpp () =
              XMPPClient.send_message xmpp
                ~jid_to:(bare_jid jid_from) ~body ?kind () in
              Queue.add (jid_from.lresource, send_message) room_env.queue
          );
          if cut then
            Hooks.make_msg xmpp (Some Chat) jid_from ?response_tail response
    | _ ->
        Hooks.make_msg xmpp kind jid_from ?response_tail response

let get_reason = function
  | None -> None
  | Some i -> i.User.reason
  
let process_presence_user ctx xmpp env stanza from room_env data enter =
  let () =
    match data.User.item with
      | None -> ()
      | Some item ->
          if stanza.content.presence_type = None then
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
          if stanza.content.presence_type = Some Unavailable then (
            hook_muc_event ctx xmpp env from
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
           if enter then
             hook_muc_event ctx xmpp env from MUC_room_created;
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
           if stanza.content.presence_type = Some Unavailable then
             let reason = get_reason data.User.item in
               hook_muc_event ctx xmpp env from (MUC_ban reason);
               true
           else
             removal
       | 303 -> (
           (* context Exiting a room *)
           (* Inform all occupants of new room nickname *)
           if stanza.content.presence_type = Some Unavailable then
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
                           hook_muc_event ctx xmpp env from
                             (MUC_nick (newnick, reason));
                           true
           else
             removal
         )
       | 307 -> (
           (* context Removal from room *)
           (* Inform user that he or she has been kicked from the room *)
           if stanza.content.presence_type = Some Unavailable then
             let reason = get_reason data.User.item in
               hook_muc_event ctx xmpp env from (MUC_kick reason);
               true
           else
             removal
           )
       | 321 -> (
           (* context Removal from room *)
           (* Inform user that he or she is being removed from
              the room because of an affiliation change *)
           if stanza.content.presence_type = Some Unavailable then
             let reason = get_reason data.User.item in
               hook_muc_event ctx xmpp env from (MUC_affiliation reason);
               true
           else
             removal
         )
       | 322 -> (
           (* context Removal from room *)
           (* Inform user that he or she is being removed from the room
              because the room has been changed to
              members-only and the user is not a member *)
           if stanza.content.presence_type = Some Unavailable then
             let reason = get_reason data.User.item in
               hook_muc_event ctx xmpp env from (MUC_members_only reason);
               true
           else
             removal
         )
       | 332 ->
           (* context Removal from room *)
           (* Inform user that he or she is being removed from the room
              because of a system shutdown *)
           if stanza.content.presence_type = Some Unavailable then
             let reason = get_reason data.User.item in
               hook_muc_event ctx xmpp env from (MUC_system_shutdown reason);
               true
           else
             removal
       | _ ->
           removal
    ) removal data.User.status in
    if stanza.content.presence_type = Some Unavailable && not removal then
      hook_muc_event ctx xmpp env from (MUC_leave stanza.content.status)

let process_presence_x ctx xmpp env stanza from room_env enter =
  List.iter (function
               | Xmlelement (qname, _, _) as el ->
                   if qname = (ns_muc_user, "x") then
                     process_presence_user ctx xmpp env stanza from room_env
                       (User.decode el) enter
               | _ ->
                   ()
            ) stanza.x
                          
let process_presence ctx xmpp env stanza hooks =
  match stanza.jid_from with
    | None -> do_hook xmpp env stanza hooks
    | Some from ->
        match opt_try (get_room_env ctx) from with
          | None -> do_hook xmpp env stanza hooks
          | Some room_env ->
              let identity jid =
                let item = Occupant.find jid.lresource room_env.occupants in
                  match item.jid with
                    | None -> jid
                    | Some j -> j
              in
              let env = {env_identity = identity;
                         env_lang = room_env.lang;
                         env_get_entity = get_entity ctx;
                         env_message = make_msg ctx;
                        }
              in
                match stanza.content.presence_type with
                  | None
                  | Some Unavailable ->
                      let enter =
                        if stanza.content.presence_type = None &&
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
                        process_presence_x ctx xmpp env stanza from
                          room_env enter;
                        if enter then
                          hook_muc_event ctx xmpp env from MUC_join;
                        if stanza.content.presence_type = Some Unavailable then
                          if from.lresource = room_env.mynick then
                            ctx.groupchats <-
                              Groupchat.remove (from.lnode, from.ldomain)
                              ctx.groupchats
                          else
                            room_env.occupants <-
                              Occupant.remove from.lresource
                              room_env.occupants;
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
             
let process_message_user ctx xmpp env stanza from data =
  let () =
    match stanza.content.message_type with
      | None
      | Some Normal -> (
          match data.User.decline with
            | None -> ()
            | Some (jid_from, jid_to, reason) ->
                hook_muc_event ctx xmpp env from
                  (MUC_decline (jid_from, jid_to, reason))
        );
          List.iter
            (fun (jid_from, jid_to, reason) ->
               hook_muc_event ctx xmpp env from
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

let do_hook_with_muc_context ctx xmpp env stanza hooks =
  match stanza.jid_from with
    | None -> do_hook xmpp env stanza hooks
    | Some from ->
        match stanza.content.message_type with
          | Some Chat -> (
              match opt_try (get_room_env ctx) from with
                | None -> do_hook xmpp env stanza hooks
                | Some room_env ->
                    match stanza.content.body with
                      | None -> do_hook xmpp env stanza hooks
                      | Some body ->
                          do_hook xmpp env stanza
                            (add_tmp_hook hooks "conversation"
                               (process_conversation "" body ctx))
            )
          | Some Groupchat -> (
              match opt_try (get_room_env ctx) from with
                | None -> do_hook xmpp env stanza hooks
                | Some room_env ->
                    if from.lresource = room_env.mynick then ( (* echo *)
                      room_env.can_send <- true;
                      if not (Queue.is_empty room_env.queue) then
                        let nick, send_message = Queue.take room_env.queue in
                          if nick = "" ||
                            Occupant.mem nick room_env.occupants then
                              send_message xmpp ()
                    )
                    else
                      match stanza.content.subject, stanza.content.body with
                        | None, Some b ->
                            if from.lresource <> "" then
                              let nick, text = split_nick_body room_env b in
                                if nick = "" then
                                  do_hook xmpp env stanza
                                    (add_tmp_hook hooks "conversation"
                                       (process_conversation nick text ctx))
                                else if nick = room_env.mynick then
                                  do_hook xmpp env
                                    {stanza with content =
                                        {stanza.content with body = Some text}}
                                    (add_tmp_hook hooks "conversation"
                                       (process_conversation nick text ctx))
                                else
                                  process_conversation nick text
                                    ctx xmpp env stanza [];
                            else
                              do_hook xmpp env stanza hooks
                        | None, None ->
                            do_hook xmpp env stanza hooks
                        | Some s, _ ->
                            do_hook xmpp env stanza hooks
            )
          | _ ->
              do_hook xmpp env stanza hooks

let process_message ctx xmpp env stanza hooks =
  match stanza.jid_from with
    | None -> do_hook xmpp env stanza hooks
    | Some from ->        
        let env =
          try            
            let room_env = get_room_env ctx from in
            let identity jid =
              let item = Occupant.find jid.lresource room_env.occupants in
                match item.jid with
                  | None -> jid
                  | Some j -> j
            in
              {env_identity = identity;
               env_lang = room_env.lang;
               env_get_entity = get_entity ctx;
               env_message = make_msg ctx;
              }
          with Not_found -> env
        in
        let continue =
          match opt_try (get_element (ns_muc_user, "x")) stanza.x with
            | Some el ->
                process_message_user ctx xmpp env stanza from (User.decode el)
            | None ->
                true
        in
          if continue then
            do_hook xmpp env stanza hooks
              
let enter_room ctx xmpp ?maxchars ?maxstanzas ?seconds ?since ?password
    ?lang ?nick room =
  let nick =
    match nick with
      | None -> (
          match ctx.default_mynick with
            | None -> xmpp.myjid.node
            | Some v -> v
        )
      | Some v -> v
  in
    ctx.groupchats <- Groupchat.add (room.lnode, room.ldomain)
      {mynick = nick;
       can_send = true;
       queue = Queue.create ();
       lang = (match lang with | None -> xmpp.user_data.deflang | Some v -> v);
       occupants = Occupant.empty} ctx.groupchats;
    MUC.enter_room xmpp ?maxchars ?maxstanzas ?seconds ?since ?password
      ~nick room

let leave_room ctx xmpp ?reason room =
  let nick = (get_room_env ctx room).mynick in
    MUC.leave_room  xmpp ?reason ~nick room
    
let change_nick xmpp jid_room newnick =
  XMPPClient.send_presence xmpp ~jid_to:(replace_resource jid_room newnick) ()
    
let invite xmpp ?reason jid_room who =
  XMPPClient.send_message xmpp ~jid_to:(bare_jid jid_room)
    ~x:[User.encode_invite ~jid_to:who ?reason ()] ()

let kick xmpp ?reason jid_room nick callback =
  XMPPClient.make_iq_request xmpp ~jid_to:(bare_jid jid_room)
    (IQSet (Admin.encode_item ~nick ~role:RoleNone ?reason ())) callback
    
let ban xmpp ?reason jid_room (jid:string) callback =
  XMPPClient.make_iq_request xmpp ~jid_to:(bare_jid jid_room)
    (IQSet (Admin.encode_item ?reason ~affiliation:AffiliationOutcast ~jid ()))
    callback

let set_topic muc_context xmpp jid_room subject =
  let room_env = get_room_env muc_context jid_room in
    if room_env.can_send then
      XMPPClient.send_message xmpp ~jid_to:(bare_jid jid_room)
        ~kind:Groupchat ~subject ()
    else
      let send_message xmpp () =
        XMPPClient.send_message xmpp ~jid_to:(bare_jid jid_room)
          ~kind:Groupchat ~subject ()
      in 
        Queue.add ("", send_message) room_env.queue;
        room_env.can_send <- false

let add_for_muc_context proc =
  ctx_hooks := proc :: !ctx_hooks
    
let get_value opts name1 name2 default =
  try List.assoc name2 (List.assoc name1 opts)
  with Not_found -> default

let get_int ?exn opts name1 name2 default =
  try let v = List.assoc name2 (List.assoc name1 opts) in
    int_of_string v
  with
    | Not_found -> default
    | Failure "int_of_string" ->
        match exn with
          | None -> default
          | Some e -> raise e

let plugin opts =
  let max_public_message_length = get_int
    ~exn:(Plugin.Error "'max_public_message_length' must be an integer")
    opts "value" "max_public_message_length" 400 in
  let file = get_value opts "db" "file" "sulci_muc.db" in
  let default_mynick =
    try Some (List.assoc "value" (List.assoc "nick" opts))
    with Not_found -> None in
    add_for_token
      (fun _opts user_data ->
         let db = db_open file in
         let ctx = {
           max_public_message_length = max_public_message_length;
           default_mynick = default_mynick;
           db = db;
           groupchats = Groupchat.empty;
           conversation_procs = [];
           muc_event_handlers = [];
         } in
           ignore (Sql.create_muc db);
           Hooks.add_message_hook user_data 10 "muc" (process_message ctx);
           Hooks.add_message_hook user_data 20 "muc_context"
             (do_hook_with_muc_context ctx);
           Hooks.add_presence_hook user_data 10 "muc" (process_presence ctx);
           List.iter (fun proc -> proc ctx user_data) (List.rev !ctx_hooks);
           register_on_connect user_data
             (fun xmpp ->
                ignore (Sql.select_rooms db
                          (fun room nick lang chatlog ->
                            enter_room ctx xmpp ~maxstanzas:0 ~lang ~nick
                               (JID.of_string room)
                          )
                       )
             )
      )

let () =
  Plugin.add_plugin "muc" plugin
