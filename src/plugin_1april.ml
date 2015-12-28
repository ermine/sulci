(*
 * (c) 2008-2010 Anastasia Gornostaeva
 *)

open XMPP
open JID
open Hooks

type rate = {
  lastrate: float;
  lasttime: float
}

let maxrate = ref 400.0

let init_rate () =
  { lastrate = 0.0; lasttime = Unix.gettimeofday ()}

(* from evgs
   S(n)=a*C/T+(1-a)*S(n-1)
   S(0)=0; 0<a<1
*)
let update_rate data size =
  let a = 0.5 in
  let now = Unix.gettimeofday () in
  let t = now -. data.lasttime in
  let s = a *. float size /. t +. (1.0 -. a) *. data.lastrate in
    {lasttime = now; lastrate = s}
      
type user = {
  reso: string;
  prio: int;
  group: int;
}
    
module User =
struct
  type t = (string * string * string)
  let compare = compare
end
  
module UserGroup = Set.Make(User)
  
type group = {
  rate: rate;
  participants: UserGroup.t
}
    
let ht = Hashtbl.create 100
let groups = Hashtbl.create 10
  
let active = ref (1, 0.0)

let new_group =
  let n = ref 1 in
    fun () -> incr n; !n
      
let log = open_out_gen [Open_append; Open_creat] 0o664 "1april.log"
  
let ltime () =
  Strftime.strftime "%d/%m/%Y %T" ~tm:(Unix.localtime (Unix.gettimeofday ()))
    
let get_activeless_group exgroup =
  Hashtbl.fold (fun gno group (g, min) ->
                  if gno <> exgroup && group.rate.lastrate < min then
                    (gno, group.rate.lastrate)
                  else
                    (g, min)
               ) groups (0, 1000.0)
    
let set_of_list list =
  List.fold_left (fun set user -> UserGroup.add user set) UserGroup.empty list
    
let update_group group users =
  List.iter (fun (user, server, _) ->
               let data = Hashtbl.find ht (user, server) in
                 Hashtbl.replace ht (user, server) {data with group = group}
            ) users
    
let divide_group xmpp group =
  let g = Hashtbl.find groups group in
  let plen = UserGroup.cardinal g.participants in
    if plen > 1 then
      let participants = UserGroup.elements g.participants in
      let rec aux_divide acc1 acc2 = function
        | 0 -> (acc1, acc2)
        | i -> aux_divide (List.tl acc1) (List.hd acc1 :: acc2) (i-1)
      in
      let g1, g2 = aux_divide participants [] (plen / 2) in
      let newgroup = new_group () in
        Hashtbl.replace groups group 
          {rate = init_rate (); participants = set_of_list g1};
        Hashtbl.add groups newgroup 
          {rate = init_rate (); participants = set_of_list g2};
        update_group newgroup g2;
        Printf.fprintf log "%s Divide group %d -> %d (%d) and %d (%d)\n"
          (ltime ()) group group (List.length g1) newgroup (List.length g2);
        flush log
          
let union_groups xmpp group1 group2 =
  let g1 = Hashtbl.find groups group1 in
  let g2 = Hashtbl.find groups group2 in
    Hashtbl.replace groups group1
      {rate = init_rate (); 
       participants = UserGroup.union g1.participants g2.participants};
    Hashtbl.remove groups group2;
    update_group group1 (UserGroup.elements g2.participants);
    Printf.fprintf log "%s Union groups %d (%d) and %d (%d)\n"
      (ltime ()) group1 (UserGroup.cardinal g1.participants)
      group2 (UserGroup.cardinal g2.participants);
    flush log
      
let add_group xmpp (group:int) ((u, s, r) as user) =
  if Hashtbl.length groups >= 2 then (
    let activeless, _ = get_activeless_group group in
    let (gno, _) = !active in
      union_groups xmpp gno activeless
  );
  try
    let g = Hashtbl.find groups group in
      Hashtbl.replace groups group
        {g with participants = UserGroup.add user g.participants}
  with Not_found ->
    Hashtbl.add groups group
      {rate = init_rate ();
       participants = UserGroup.add user UserGroup.empty}
      
let remove_group xmpp (group:int) user =
  let g = Hashtbl.find groups group in
  let participants = UserGroup.remove user g.participants in
    if UserGroup.is_empty participants then
      Hashtbl.remove groups group
    else
      Hashtbl.replace groups group {g with participants = participants}
        
let add_user xmpp (jid:jid) prio =
  try 
    let data = Hashtbl.find ht (jid.lnode, jid.ldomain) in
      if data.reso = jid.lresource then (
        if data.prio <> prio then (
          Hashtbl.replace ht (jid.lnode, jid.ldomain) 
            {data with prio = prio};
        )
      )
      else if prio > data.prio then
        let old = data.reso in
          Printf.fprintf log "%s Replaced user [%d] %s@%s: (%s) -> (%s)\n"
            (ltime ()) data.group jid.node jid.domain old jid.resource;
          flush log;
          Hashtbl.replace ht (jid.lnode, jid.ldomain)
            {data with reso = jid.lresource; prio = prio};
          XMPP.send_presence xmpp
            ~jid_to:(replace_resource jid old)
            ~kind:Unavailable ();
          remove_group xmpp data.group (jid.lnode, jid.ldomain, old);
          add_group xmpp data.group (jid.lnode, jid.ldomain, jid.lresource);
  with Not_found ->
    let group, _ = !active in
      Printf.fprintf log "%s New participant: [%d] (%s@%s/%s)\n" 
        (ltime ()) group jid.node jid.domain jid.resource;
      flush log;
      Hashtbl.add ht (jid.lnode, jid.ldomain) 
        { reso = jid.lresource; prio = prio;
          group = group };
      add_group xmpp group (jid.lnode, jid.ldomain, jid.lresource)
        
let remove_user xmpp jid =
  try
    let data = Hashtbl.find ht (jid.lnode, jid.ldomain) in
      if data.reso = jid.lresource then (
        Printf.fprintf log "%s Remove participant: [%d] (%s@%s/%s)\n" 
          (ltime ()) data.group jid.lnode jid.ldomain jid.lresource;
        flush log;
        Hashtbl.remove ht (jid.lnode, jid.ldomain);
        remove_group xmpp data.group (jid.lnode, jid.ldomain, jid.lresource);
        XMPP.send_presence xmpp
          ~jid_to:jid
          ~kind:Unavailable ()
      )
  with Not_found ->
    ()
      
let dispatch xmpp from body =
  try
    let data = Hashtbl.find ht (from.lnode, from.ldomain) in
    let group = Hashtbl.find groups data.group in
    let newrate = update_rate group.rate 100 in
      Printf.fprintf log "%s Message [%d] (%g) (%s@%s/%s)\n%s\n\n"
        (ltime ()) data.group group.rate.lastrate 
        from.node from.domain from.resource body;
      flush log;
      Hashtbl.replace groups data.group {group with rate = newrate};
      if newrate.lastrate >= !maxrate then
        divide_group xmpp data.group;
      let gno, grate = !active in
        if newrate.lastrate > grate then
          active := (data.group, newrate.lastrate);
        let group = Hashtbl.find groups data.group in
          UserGroup.iter
            (fun (lnode, ldomain, lresource) ->
               if (lnode, ldomain, lresource) = 
                 (from.lnode, from.ldomain, from.lresource) then
                   ()
               else
                   XMPP.send_message xmpp
                     ~jid_to:(make_jid lnode ldomain lresource)
                     ~kind:Chat
                     ~body ()
            ) group.participants
  with Not_found ->
    Printf.fprintf log "%s Message from not-logged in user (%s@%s/%s)\n"
      (ltime ()) from.lnode from.ldomain from.lresource;
    flush log
      
let process_presence_error xmpp ?id ?jid_from ?jid_to ?lang error =
  match jid_from with
    | None -> ()
    | Some from ->
        Printf.fprintf log "%s Presence error (%s@%s/%s)\n"
          (ltime ()) from.node from.domain from.resource;
        flush log;
        remove_user xmpp from

let process_presence xmpp env stanza hooks =
  match stanza.jid_from with
    | None -> do_hook xmpp env stanza hooks
    | Some from ->
        let () =
          if from.lnode = xmpp.myjid.lnode &&
            from.ldomain = xmpp.myjid.ldomain then
              ()
          else
            match stanza.content.presence_type with
              | Some Subscribe ->
                  Printf.fprintf log "%s Subscribe (%s@%s)\n" 
                    (ltime ()) from.node from.domain;
                  flush log;
                  XMPP.send_presence xmpp
                    ~jid_to:(replace_resource from "")
                    ~kind:Subscribed ();
                  XMPP.send_presence xmpp
                    ~jid_to:(replace_resource from "")
                    ~kind:Subscribe ();
              | Some Subscribed ->
                  Printf.fprintf log "%s Unsubscribe (%s@%s)\n" 
                    (ltime ()) from.node from.domain;
                  flush log;
                  XMPP.send_presence xmpp
                    ~jid_to:(replace_resource from "")
                    ~kind:Unsubscribed ()
              | None ->
                  Printf.fprintf log 
                    "%s Presence available (%s@%s/%s)\n"
                    (ltime ()) from.node from.domain from.resource;
                  flush log;
                  let prio =
                    match stanza.content.XMPP.priority with
                      | None -> 0
                      | Some i -> i
                  in
                    add_user xmpp from prio
              | Some Unavailable ->
                  Printf.fprintf log 
                    "%s Presence unavailable (%s@%s/%s)\n"
                    (ltime ()) from.node from.domain from.resource;
                  flush log;
                  remove_user xmpp from
              | _ ->
                  ()
        in
          do_hook xmpp env stanza hooks

let process_message_error xmpp ?id ?jid_from ?jid_to ?lang error =
  match jid_from with
    | None -> ()
    | Some from ->
        Printf.fprintf log "%s Presence error (%s@%s/%s)\n"
          (ltime ()) from.node from.domain from.resource;
        flush log;
        remove_user xmpp from
    
let process_message xmpp env stanza hooks =
  match stanza.jid_from with
    | None -> do_hook xmpp env stanza hooks
    | Some from ->
        let () =
          match stanza.content.message_type, stanza.content.body with
            | Some Chat, Some body ->
                if body <> "" && String.length body < 1024 then
                  dispatch xmpp from body
            | _ ->
                ()
        in
          do_hook xmpp env stanza hooks
            
let plugin opts =
  Hooks.add_for_token
    (fun _opts xmpp ->
       Hooks.add_message_hook xmpp 30 "1april" process_message;
       Hooks.add_presence_hook xmpp 30 "1april" process_presence
    )
    
let () =
  Plugin.add_plugin "1april" plugin
    
