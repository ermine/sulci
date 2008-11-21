(*
 * (c) 2004-2008 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Common
open Xml
open Xmpp
open Jid
open Types
open Nicks

let process_presence (from:jid) xml out =
  let room = from.lnode, from.ldomain in
  let lnode = from.lresource in
  let room_env = GroupchatMap.find room !groupchats in
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
                   nicks = Nicks.add lnode newitem
                    room_env.nicks} !groupchats;
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
                       {room_env with mynick = newnick; 
                          nicks = new_items}
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
  let room = from.lnode, from.ldomain in
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
                  let room_env = GroupchatMap.find room !groupchats in
                  let nick, text = split_nick_body room_env body in
                    MUC_message (msg_type, nick, text)
              | _ ->
                  MUC_message (msg_type, "", body)
        with Not_found ->
          MUC_other
            
let invite ?reason (lnode, ldomain) who =
  make_message ~to_: (lnode ^ "@" ^ ldomain) 
    ~subels:[Xmlelement ("x", 
                         ["xmlns", "http://jabber.org/protocol/muc#user"],
                         [Xmlelement ("invite", ["to", who], 
                                      (match reason with
                                         | None -> []
                                         | Some r -> 
                                             [make_simple_cdata "reason" r]
                                      ))])] ()
    
let join_room nick (lnode, ldomain) =
  make_presence ~to_:(lnode ^ "@" ^ ldomain ^ "/" ^ nick)
    ~subels:
    [Xmlelement ("x", ["xmlns", "http://jabber.org/protocol/muc"], [])] ()
    
let leave_room ?reason (lnode, ldomain) =
  let mynick = (GroupchatMap.find (lnode, ldomain) !groupchats).mynick in
    make_presence ~to_:(lnode ^ "@" ^ ldomain ^ "/" ^ mynick) 
      ~type_:`Unavailable ?status:reason ()
      
let kick ?reason id (lnode, ldomain) nick =
  make_iq ~id ~to_:(lnode ^ "@" ^ ldomain) ~type_:`Set
    ~xmlns:"http://jabber.org/protocol/muc#admin"
    ~subels:[Xmlelement ("item", ["nick", nick; "role", "none"],
                         (match reason with
                            | None -> []
                            | Some r ->
                                [make_simple_cdata "reason" r])
                        )] ()
    
let ban id ?reason (lnode,ldomain) (jid:string) =
  make_iq ~id ~to_:(lnode ^ "@" ^ ldomain)
    ~xmlns:"http://jabber.org/protocol/muc#admin" ~type_:`Set
    ~subels:[Xmlelement ("item", ["affiliation", "outcast"; "jid", jid], 
                         (match reason with
                            | None -> []
                            | Some r ->
                                [make_simple_cdata "reason" r]))] ()
    
let set_topic from subject =
  Xmlelement ("message", ["to", string_of_jid (bare_jid from);
                          "type", "groupchat"],
              [make_simple_cdata "subject" subject])
    
let register_room ?lang nick (lnode, ldomain) =
  groupchats := GroupchatMap.add (lnode, ldomain)
    {
      mynick = Stringprep.stringprep ~mode:Stringprep.Resourceprep nick;
      nicks = [];
      lang = match lang with
        | None -> Lang.deflang
        | Some l -> l } !groupchats
    
let _ =
  let default_mynick = 
    trim (Xml.get_cdata Config.config ~path:["jabber"; "user"]) in
  let rconf = 
    try Xml.get_subels ~path:["muc"] ~tag:"room" Config.config with _ -> [] in
    
    List.iter 
      (fun r ->
         let mynick = try 
           Stringprep.stringprep ~mode:Stringprep.Resourceprep
             (Xml.get_attr_s r "nick") with Not_found -> default_mynick
         and jid_s = Xml.get_attr_s r "jid"
         and lang = try Xml.get_attr_s r "lang" with _ -> "ru" in
         let jid = jid_of_string jid_s in
           register_room ~lang mynick (jid.lnode, jid.ldomain)
      ) rconf
     
