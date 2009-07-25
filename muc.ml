(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open XMPP
open Jid
open Types
open Config
open Common
open Hooks
open Muc_types
open Nicks
open Sqlite3
open Sqlite_util

let ns_muc = Some "http://jabber.org/protocol/muc"
let ns_muc_admin = Some "http://jabber.org/protocol/muc#admin"
let ns_muc_user = Some  "http://jabber.org/protocol/muc#user"

let catchers = ref []
let filters:
    (string, (Muc_types.muc_event -> Jid.jid ->
                element -> Types.local_env -> out -> unit)) Hashtbl.t
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
    match safe_get_attr_value "type" (get_attrs xml) with
      | "groupchat" ->
          (GroupchatMap.find (from.lnode, from.ldomain) !groupchats).lang
      | _ ->
          Lang.get_lang xml
  else
    Lang.get_lang xml

let do_catcher event from xml env out =
  List.iter (fun f -> try f event from xml env out with exn ->
               log#error "[executing catch callback] %s"
                 (Printexc.to_string exn);
               log#debug "%s" (Printexc.get_backtrace ())
            ) !catchers

let is_echo from =
  let room_env = get_room_env from in
    room_env.mynick = from.lresource

let process_presence (from:jid) xml _out =
  let lnode = from.lresource in
  let room = from.lnode, from.ldomain in
  let room_env = get_room_env from in
  let x = get_subelement (ns_muc_user, "x") xml in
  let item_el =
    try Some (get_subelement (ns_muc_user, "item") x) with Not_found -> None in
  let type_ = get_presence_type xml in
  let status = get_presence_status ~ns:ns_client xml in
    match type_ with
      | `Available -> (
          let show = get_presence_show ~ns:ns_client xml in
            try
              let item = Nicks.find lnode room_env.nicks in
              let newitem = 
                {item with 
                   status = status; 
                   show = show;
                   role = (
                    match item_el with
                      | None -> item.role
                      | Some el ->
                          match safe_get_attr_value "role" (get_attrs el) with
                            | "moderator" -> `Moderator 
                            | "participant" -> `Participant 
                            | "visitor" -> `Visitor 
                            | _ -> item.role);
                   affiliation =
                    match item_el with
                      | None -> item.affiliation
                      | Some el ->
                          match safe_get_attr_value "affiliation" (get_attrs el)
                          with
                            | "owner" -> `Owner 
                            | "admin" -> `Admin 
                            | "member" -> `Member 
                            | "outcast" -> `Outcast
                            | _ -> item.affiliation
                } in
                groupchats := GroupchatMap.add room 
                  {room_env with 
                     nicks = Nicks.add lnode newitem room_env.nicks} !groupchats;
                MUC_presence newitem
            with Not_found ->
              let item_el = get_subelement (ns_muc_user, "item") x in
              let item = { 
                jid = (try Some (jid_of_string
                                   (get_attr_value "jid" (get_attrs item_el)))
                       with _ -> None);
                role = (
                  match try get_attr_value "role" (get_attrs item_el)
                           with _ -> "" with
                             | "moderator" -> `Moderator 
                             | "participant" -> `Participant 
                             | "visitor" -> `Visitor 
                             | _ -> `None
                );
                affiliation = (
                  match try get_attr_value "affiliation" (get_attrs item_el)
                  with _ -> "" with
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
          let item_el = get_subelement (ns_muc_user, "item") x in
          let reason =
            try get_cdata (get_subelement (ns_muc_user, "reason") item_el)
            with Not_found -> "" in
            (match try get_attr_value "code"
               (get_attrs (get_subelement (ns_muc_user, "status") x))
             with Not_found -> "" with
               | "303" -> (* /nick *)
                   let newnick = 
                     Stringprep.resourceprep 
                       (get_attr_value "nick" (get_attrs item_el)) in
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
      | _ ->
          MUC_other
          
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
      
let process_message (from:jid) xml _out = 
  if mem_qname (Some "jabber:x:delay", "x") (get_children xml) then
    MUC_history
  else
    try
      let subject = get_cdata (get_subelement (ns_client, "subject")  xml) in
        MUC_topic subject
    with Not_found ->
      try 
        let body = get_cdata (get_subelement (ns_client, "body") xml) in
        let msg_type = 
          try match get_type xml with
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
    | MUC_presence _
    | MUC_topic _
    | MUC_history
    | MUC_other
    | MUC_join _
    | MUC_change_nick _
    | MUC_leave _ ->
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
  let tag = get_name (get_qname xml) in
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
            Hooks.process_iq from xml env out
        | _ ->
            () (* todo: log *)
    else
      Hooks.default_dispatch_xml from xml out

let invite ?reason (lnode, ldomain) who =
  make_message ~ns:ns_client ~jid_to: (lnode ^ "@" ^ ldomain) 
    ~subels:[make_element (ns_muc_user, "x") []
               [make_element (ns_muc_user, "invite")
                  [make_attr "to" who]
                  (match reason with
                     | None -> []
                     | Some r ->
                         [make_simple_cdata (ns_muc_user, "reason") r])]] ()
    
let join_room nick (lnode, ldomain) =
  make_presence ~ns:ns_client ~jid_to:(lnode ^ "@" ^ ldomain ^ "/" ^ nick)
    ~subels:[make_element (ns_muc, "x") [] []] ()
    
let leave_room ?reason (lnode, ldomain) =
  let mynick = (GroupchatMap.find (lnode, ldomain) !groupchats).mynick in
    make_presence ~ns:ns_client ~jid_to:(lnode ^ "@" ^ ldomain ^ "/" ^ mynick) 
      ~type_:`Unavailable ?status:reason ()
      
let kick ?reason id (lnode, ldomain) nick =
  make_iq ~ns:ns_client ~id ~jid_to:(lnode ^ "@" ^ ldomain) ~type_:`Set
    ~payload:[make_element (ns_muc_admin, "query") []
                [make_element (ns_muc_admin, "item")
                   [make_attr "nick" nick;
                    make_attr "role" "none"]
                   (match reason with
                      | None -> []
                      | Some r ->
                          [make_simple_cdata (ns_muc_admin, "reason")
                             r])]] ()
    
let ban id ?reason (lnode,ldomain) (jid:string) =
  make_iq ~ns:ns_client ~id ~jid_to:(lnode ^ "@" ^ ldomain) ~type_:`Set
    ~payload:[make_element (ns_muc_admin, "query") []
                [make_element (ns_muc_admin, "item")
                   [make_attr "affiliation" "outcast";
                    make_attr "jid" jid]
                   (match reason with
                      | None -> []
                      | Some r ->
                          [make_simple_cdata (ns_muc_admin, "reason")
                             r])]] ()
    
let set_topic from subject =
  make_element (ns_client, "message")
    [make_attr "to" (string_of_jid (bare_jid from));
     make_attr "type" "groupchat"]
    [make_simple_cdata (ns_client, "subject") subject]
    
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
      

let load_rooms out =
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
            if chatlog then
              Muc_log.add_chatlog room;
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

let _ =
  Hooks.register_dispatch_xml dispatch_xml;
  Hooks.register_on_connect load_rooms;
  Hooks.register_on_disconnect (fun () -> groupchats := GroupchatMap.empty)

