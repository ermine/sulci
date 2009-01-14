(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open Xmpp
open Jid
open Types
open Config
open Common
open Hooks
open Muc_types
open Nicks
open Sqlite3
open Sqlite_util

let catchers = ref []
let filters:
    (string, (Muc_types.muc_event -> Jid.jid ->
                Xml.element -> Types.local_env ->
                  (Xml.element -> unit) -> unit)) Hashtbl.t
    = Hashtbl.create 5

let register_catcher proc =
  catchers := proc :: !catchers

let register_filter name proc =
  Hashtbl.replace filters name proc

let is_groupchat jid =
  GroupchatMap.mem (jid.lnode, jid.ldomain) !groupchats

let get_room_env jid =
  GroupchatMap.find (jid.lnode, jid.ldomain) !groupchats

let get_lang from xml =
  if is_groupchat from then
    match safe_get_attr_s xml "type" with
      | "groupchat" ->
          (GroupchatMap.find (from.lnode, from.ldomain) !groupchats).lang
      | _ ->
          Lang.get_lang xml
  else
    Lang.get_lang xml

let do_catcher event from xml env out =
  List.iter  (fun f -> 
                try f event from xml env out with exn ->
                  log#error "[executing catch callback] %s: %s"
                    (Printexc.to_string exn)
                    (element_to_string xml)
             ) !catchers 

let is_echo from =
  let room_env = get_room_env from in
    room_env.mynick = from.lresource

let process_presence (from:jid) xml out =
  let lnode = from.lresource in
  let room = from.lnode, from.ldomain in
  let room_env = get_room_env from in
  let x = get_by_xmlns xml ~tag:"x" "http://jabber.org/protocol/muc#user" in
  let type_, status = presence_info xml in
    match type_ with
      | `Available show -> (
          try
            let item = Nicks.find lnode room_env.nicks in
            let newitem = 
              {item with 
                 status = status; 
                 show = show;
                 role =
                  (try match get_attr_s x ~path:["item"] "role" with
                     | "moderator" -> `Moderator 
                     | "participant" -> `Participant 
                     | "visitor" -> `Visitor 
                     | _ -> `None
                   with Not_found -> item.role);
                 affiliation =
                  (try match 
                     get_attr_s x ~path:["item"] "affiliation" with
                       | "owner" -> `Owner 
                       | "admin" -> `Admin 
                       | "member" -> `Member 
                       | "outcast" -> `Outcast 
                       | _ -> `None
                   with Not_found -> item.affiliation)
              } in
              groupchats := GroupchatMap.add room 
                {room_env with 
                   nicks = Nicks.add lnode newitem room_env.nicks} !groupchats;
              MUC_presence newitem
          with Not_found -> 
            let item = { 
              jid = (try Some (jid_of_string 
                                 (get_attr_s x ~path:["item"] "jid"))
                     with _ -> None);
              role = (
                let v = (try get_attr_s x ~path:["item"] "role" 
                         with _ -> "") in
                  match v with
                    | "moderator" -> `Moderator 
                    | "participant" -> `Participant 
                    | "visitor" -> `Visitor 
                    | _ -> `None
              );
              affiliation = (
                let v = (try get_attr_s x 
                           ~path:["item"] "affiliation"
                         with _ -> "") in
                  match v with
                    | "owner" -> `Owner 
                    | "admin" -> `Admin 
                    | "member" -> `Member 
                    | "outcast" -> `Outcast 
                    | _ -> `None
              );
              status = status;
              show = show;
              orig_nick = lnode
            } in
              groupchats := GroupchatMap.add room 
                {room_env with nicks = Nicks.add lnode item
                    room_env.nicks} !groupchats;
              MUC_join item
        )
      | `Unavailable ->
          let item = Nicks.find lnode room_env.nicks in
          let reason = 
            try get_cdata ~path:["item";"reason"] x with _ -> "" in
            (match safe_get_attr_s x ~path:["status"] "code" with
               | "303" -> (* /nick *)
                   let newnick = 
                     Stringprep.resourceprep 
                       (get_attr_s xml ~path:["x"; "item"] "nick") in
                   let new_items = Nicks.add newnick item 
                     (Nicks.remove lnode room_env.nicks) in
                   let new_room_env =
                     if lnode = room_env.mynick then
                       {room_env with mynick = newnick; nicks = new_items}
                     else
                       {room_env with nicks = new_items} in
                     groupchats := GroupchatMap.add room new_room_env
                       !groupchats;
                     MUC_change_nick (newnick, item)
               | "307" -> (* /kick *)
                   if lnode = room_env.mynick then (
                     groupchats := GroupchatMap.remove room !groupchats;
                     MUC_leave (true, `Kick, reason, item)
                   ) else (
                     groupchats := GroupchatMap.add room
                       {room_env with nicks =
                           Nicks.remove lnode 
                             room_env.nicks} !groupchats;
                     MUC_leave (false, `Kick, reason, item)
                   )
               | "301" -> (* /ban *)
                   if lnode = room_env.mynick then (
                     groupchats := GroupchatMap.remove room !groupchats;
                     MUC_leave (true, `Ban, reason, item)
                   ) else (
                     groupchats := GroupchatMap.add room
                       {room_env with nicks =
                           Nicks.remove lnode 
                             room_env.nicks} !groupchats;
                     MUC_leave (false, `Ban, reason, item)
                   )
               | "321" -> (* non-member *)
                   if lnode = room_env.mynick then (
                     groupchats := GroupchatMap.remove room !groupchats;
                     MUC_leave (true, `UnMember, reason, item)
                   ) else (
                     groupchats := GroupchatMap.add room
                       {room_env with nicks =
                           Nicks.remove lnode 
                             room_env.nicks} !groupchats;
                     MUC_leave (false, `UnMember, reason, item)
                   )
               | ""
               | _ ->
                   if lnode = room_env.mynick then (
                     groupchats := GroupchatMap.remove room !groupchats;
                     MUC_leave (true, `Normal, status, item)
                   ) else (
                     groupchats := GroupchatMap.add room
                       {room_env with nicks =
                           Nicks.remove lnode 
                             room_env.nicks} !groupchats;
                     MUC_leave (false, `Normal, status, item)
                   )
            )
      | _ -> MUC_other
          
let split_nick_body room_env body =
  let rec cycle nicks =
    match nicks with
      | [] -> "", body
      | (nick, _) :: xs ->
          let len_nick = String.length nick in
          let len_body = String.length body in
            if len_nick = len_body && nick = body then
              body, ""
            else if len_body < len_nick then
              cycle xs
            else if String.sub body 0 len_nick = nick then
              if List.mem body.[len_nick] [':'; ','; '.'; '>'] then
                if len_nick + 1 = len_body then
                  nick, ""
                else if body.[len_nick+1] = ' ' then
                  nick, string_after body (len_nick+2)
                else
                  cycle xs
              else if len_nick + 4 < len_body && 
                String.sub body len_nick 5 = "&gt; " then
                  nick, string_after body (len_nick+5)
              else
                cycle xs
            else
              cycle xs
  in
    cycle room_env.nicks
      
let process_message (from:jid) xml out = 
  if (mem_xml xml ["message"] "x" ["xmlns", "jabber:x:delay"]) then
    MUC_history
  else
    try
      let subject = get_cdata xml ~path:["subject"] in
        MUC_topic subject
    with Not_found ->
      try 
        let body = get_cdata xml ~path:["body"] in
        let msg_type = 
          try match get_attr_s xml "type" with
            | "groupchat" -> `Groupchat
            | "chat" -> `Chat
            | "error" -> `Error
            | _ -> `Normal
          with _ -> `Normal in
          match msg_type with
            | `Groupchat ->
                let room_env = get_room_env from in
                let nick, text = split_nick_body room_env body in
                  MUC_message (msg_type, nick, text)
            | _ ->
                MUC_message (msg_type, "", body)
      with Not_found ->
        MUC_other
            

let process_plugins event from xml env out =
  match event with
    | MUC_message (msg_type, nick, text) ->
        if not (is_echo from) then
          if msg_type <> `Error then
            if text <> "" && nick = "" then
              try do_command text from xml env out
              with Not_found ->
                do_catcher event from xml env out
            else
              do_catcher event from xml env out
          else
            () (* callback? *)
        else
          () (* echo *)
    | _ ->
        do_catcher event from xml env out

let process_event event from xml env out =
  Muc_log.process_log event from xml env;
  let room_env = get_room_env from in
  let good =
    match room_env.filter with
      | Some f ->
          (try f event from xml env out; true with Filtered -> false)
      | None -> true
  in
    if good then
      process_plugins event from xml env out

let muc_check_access (jid:jid) classname =
  try 
    let room_env = get_room_env jid in
    let nick = jid.lresource in
    let item = Nicks.find nick room_env.nicks in
      match item.jid with
        | None -> false (* TODO? *)
        | Some j -> Hooks.default_check_access j classname
  with Not_found ->
    Hooks.default_check_access jid classname

let muc_get_entity text from =
  if text = "" then
    EntityYou, from
  else
    let room_env = get_room_env from in
      if room_env.mynick = text then
        EntityMe, {from with resource = text; lresource = text}
      else if Nicks.mem text room_env.nicks then
        if from.resource = text then
          EntityYou, from
        else
          EntityUser, {from with resource = text; lresource = text}
      else
        try
          let jid = jid_of_string text in
            if Jid.equal jid from then
              EntityYou, jid
            else if jid.lnode = "" then (
              dnsprep jid.ldomain;
              EntityHost, jid
            )
            else
              EntityUser, jid
        with _ ->
          raise BadEntity
            
let dispatch_xml from xml out =
  let groupchat = is_groupchat from in
  let env = {env_groupchat = true;
             env_lang = get_lang from xml;
             env_check_access = muc_check_access;
             env_get_entity = muc_get_entity } in
  let tag = get_tagname xml in
    if groupchat then
      match tag with
        | "message" ->
            let event = process_message from xml out in
              process_event event from xml env out
        | "presence" ->
          if groupchat then
            let event = process_presence from xml out in
              process_event event from xml env out
        | "iq" ->
            Hooks.process_iq from xml out
        | _ ->
            () (* todo: log *)
    else
      Hooks.default_dispatch_xml from xml out

let invite ?reason (lnode, ldomain) who =
  make_message ~to_: (lnode ^ "@" ^ ldomain) 
    ~subels:[make_element "x"
               ["xmlns", "http://jabber.org/protocol/muc#user"]
               [make_element "invite" ["to", who]
                  (match reason with
                     | None -> []
                     | Some r -> [make_simple_cdata "reason" r])]] ()
    
let join_room nick (lnode, ldomain) =
  make_presence ~to_:(lnode ^ "@" ^ ldomain ^ "/" ^ nick)
    ~subels:[make_element "x" ["xmlns", "http://jabber.org/protocol/muc"] []] ()
    
let leave_room ?reason (lnode, ldomain) =
  let mynick = (GroupchatMap.find (lnode, ldomain) !groupchats).mynick in
    make_presence ~to_:(lnode ^ "@" ^ ldomain ^ "/" ^ mynick) 
      ~type_:`Unavailable ?status:reason ()
      
let kick ?reason id (lnode, ldomain) nick =
  make_iq ~id ~to_:(lnode ^ "@" ^ ldomain) ~type_:`Set
    ~xmlns:"http://jabber.org/protocol/muc#admin"
    ~subels:[make_element "item" ["nick", nick; "role", "none"]
               (match reason with
                  | None -> []
                  | Some r -> [make_simple_cdata "reason" r])] ()
    
let ban id ?reason (lnode,ldomain) (jid:string) =
  make_iq ~id ~to_:(lnode ^ "@" ^ ldomain)
    ~xmlns:"http://jabber.org/protocol/muc#admin" ~type_:`Set
    ~subels:[make_element "item" ["affiliation", "outcast"; "jid", jid]
               (match reason with
                  | None -> []
                  | Some r -> [make_simple_cdata "reason" r])] ()
    
let set_topic from subject =
  make_element "message" ["to", string_of_jid (bare_jid from);
                          "type", "groupchat"]
    [make_simple_cdata "subject" subject]
    
let register_room ?lang ?filter nick jid  =
  groupchats := GroupchatMap.add (jid.lnode, jid.ldomain)
    {
      mynick = Stringprep.stringprep ~mode:Stringprep.Resourceprep nick;
      nicks = [];
      lang = (match lang with
                | None -> Lang.deflang
                | Some l -> l);
      filter = (match filter with
                  | None -> None
                  | Some v ->
                      try Some (Hashtbl.find filters v) with Not_found -> None)
    } !groupchats
    
let file =
  try trim (Xml.get_cdata Config.config ~path:["muc"; "db"])
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
      

let load_rooms out =
  let sql = Printf.sprintf "SELECT room, nick, lang, chatlog, filter FROM %s"
    table in
  let rec iter_room stmt =
    match step stmt with
      | Rc.ROW ->
          let room = Jid.jid_of_string (Data.to_string (column stmt 0)) in
          let mynick = Data.to_string (column stmt 1) in
          let lang = Data.to_string (column stmt 2) in
          let chatlog =
            match Data.to_string (column stmt 3) with
              | "T" -> true
              | _ -> false
          in
          let filter = Data.to_string (column stmt 4) in
            if chatlog then
              Muc_log.add_chatlog room;
            register_room ~lang ~filter mynick room;
            out (join_room mynick (room.lnode, room.ldomain)); 
            iter_room stmt
      | Rc.DONE -> ()
      | _ -> exit_with_rc file db sql
  in
    try
      let stmt = prepare db sql in
        iter_room stmt
    with Sqlite3.Error _ ->
      exit_with_rc file db sql

let _ =
  Hooks.register_dispatch_xml dispatch_xml;
  Hooks.register_on_connect load_rooms;
  Hooks.register_on_disconnect (fun () -> groupchats := GroupchatMap.empty)

