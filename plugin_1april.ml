(*
 * (c) 2008 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Xml
open Xmpp

open Types
open Hooks
open Common

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
    
let str_of_jid user server resource = 
  user ^ "@" ^ server ^ (if resource = "" then "" else "/" ^ resource)

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
    
let divide_group group out =
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
          
let union_groups group1 group2 out =
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
      
let add_group (group:int) ((u, s, r) as user) out =
  if Hashtbl.length groups >= 2 then (
    let activeless, _ = get_activeless_group group in
    let (gno, _) = !active in
      union_groups gno activeless out
  );
  try
    let g = Hashtbl.find groups group in
      Hashtbl.replace groups group
        {g with participants = UserGroup.add user g.participants}
  with Not_found ->
    Hashtbl.add groups group
      {rate = init_rate ();
       participants = UserGroup.add user UserGroup.empty}
      
let remove_group (group:int) user out =
  let g = Hashtbl.find groups group in
  let participants = UserGroup.remove user g.participants in
    if UserGroup.is_empty participants then
      Hashtbl.remove groups group
    else
      Hashtbl.replace groups group {g with participants = participants}
        
let add_user (jid:jid) prio out =
  try 
    let data = Hashtbl.find ht (jid.luser, jid.lserver) in
      if data.reso = jid.lresource then (
        if data.prio <> prio then (
          Hashtbl.replace ht (jid.luser, jid.lserver) 
            {data with prio = prio};
        )
      )
      else if prio > data.prio then
        let old = data.reso in
          Printf.fprintf log "%s Replaced user [%d] %s@%s: (%s) -> (%s)\n"
            (ltime ()) data.group jid.user jid.server old jid.resource;
          flush log;
          Hashtbl.replace ht (jid.luser, jid.lserver)
            {data with reso = jid.lresource; prio = prio};
          out (make_presence 
                 ~to_:(str_of_jid jid.luser jid.lserver old)
                 ~type_:`Unavailable ());
          remove_group data.group (jid.luser, jid.lserver, old) out;
          add_group data.group (jid.luser, jid.lserver, jid.lresource) out;
  with Not_found ->
    let group, _ = !active in
      Printf.fprintf log "%s New participant: [%d] (%s@%s/%s)\n" 
        (ltime ()) group jid.user jid.server jid.resource;
      flush log;
      Hashtbl.add ht (jid.luser, jid.lserver) 
        { reso = jid.lresource; prio = prio;
          group = group };
      add_group group (jid.luser, jid.lserver, jid.lresource) out
        
let remove_user jid out =
  try
    let data = Hashtbl.find ht (jid.luser, jid.lserver) in
      if data.reso = jid.lresource then (
        Printf.fprintf log "%s Remove participant: [%d] (%s@%s/%s)\n" 
          (ltime ()) data.group jid.luser jid.lserver jid.lresource;
        flush log;
        Hashtbl.remove ht (jid.luser, jid.lserver);
        remove_group data.group (jid.luser, jid.lserver, jid.lresource) out;
        out (make_presence 
               ~to_:(str_of_jid jid.user jid.server jid.resource)
               ~type_:`Unavailable ())
      )
  with Not_found ->
    ()
      
let dispatch from body_s out =
  try
    let data = Hashtbl.find ht (from.luser, from.lserver) in
    let group = Hashtbl.find groups data.group in
    let newrate = update_rate group.rate 100 in
      Printf.fprintf log "%s Message [%d] (%g) (%s@%s/%s)\n%s\n\n"
        (ltime ()) data.group group.rate.lastrate 
        from.user from.server from.resource body_s;
      flush log;
      Hashtbl.replace groups data.group {group with rate = newrate};
      if newrate.lastrate >= !maxrate then
        divide_group data.group out;
      let body = make_simple_cdata "body" body_s in
      let gno, grate = !active in
        if newrate.lastrate > grate then
          active := (data.group, newrate.lastrate);
        let group = Hashtbl.find groups data.group in
          UserGroup.iter
            (fun (luser, lserver, lresource) ->
               if (luser, lserver, lresource) = 
                 (from.luser, from.lserver, from.lresource) then
                   ()
               else
                 let jid = luser ^ "@" ^ lserver ^ "/" ^ lresource in
                   out (Xmlelement ("message", 
                                    ["to", jid; "type", "chat"],
                                    [body]))
            ) group.participants
  with Not_found ->
    Printf.fprintf log "%s Message from not-logged in user (%s@%s/%s)\n"
      (ltime ()) from.luser from.lserver from.lresource;
    flush log
      
let my_jid = 
  let username = trim (Xml.get_cdata Config.config ~path:["jabber"; "user"]) in
  let server = trim (Xml.get_cdata Config.config ~path:["jabber"; "server"]) in
    (*
      let resource = 
      trim (Xml.get_cdata Config.config ~path:["jabber"; "resource"]) in
    *)
    username, server
      

let catch_1april event from xml out =
  if (from.luser, from.lserver) = my_jid then
    ()
  else
    match event with
      | Presence ->
          let t = safe_get_attr_s xml "type" in
            (match t with
               | "subscribe" ->
                   Printf.fprintf log "%s Subscribe (%s@%s)\n" 
                     (ltime ()) from.user from.server;
                   flush log;
                   out (make_presence 
                          ~to_:(str_of_jid from.user from.server "")
                          ~type_:`Subscribed ());
                   out (make_presence
                          ~to_:(str_of_jid from.user from.server "")
                          ~type_:`Subscribe ());
               | "unsubscribed" ->
                   Printf.fprintf log "%s Unsubscribe (%s@%s)\n" 
                     (ltime ()) from.user from.server;
                   flush log;
                   out (make_presence 
                          ~to_:(str_of_jid from.user from.server "")
                          ~type_:`Unsubscribed ())
               | "" ->
                   Printf.fprintf log 
                     "%s Presence available (%s@%s/%s)\n"
                     (ltime ()) from.user from.server from.resource;
                   flush log;
                   let prio = 
                     try 
                       int_of_string (get_cdata xml 
                                        ~path:["priority"])
                     with _ -> 0 in
                     add_user from prio out
               | "error"->
                   Printf.fprintf log "%s Presence error (%s@%s/%s)\n"
                     (ltime ()) from.user from.server from.resource;
                   flush log;
                   remove_user from out
               | "unavailable" ->
                   Printf.fprintf log 
                     "%s Presence unavailable (%s@%s/%s)\n"
                     (ltime ()) from.user from.server from.resource;
                   flush log;
                   remove_user from out
               | _ ->
                   ()
            )
      | Message ->
          if safe_get_attr_s xml "type" = "error" then (
            Printf.fprintf log "%s Presence error (%s@%s/%s)\n"
              (ltime ()) from.user from.server from.resource;
            flush log;
            remove_user from out
          ) else
            let body = 
              try get_cdata xml ~path:["body"]
              with Not_found -> "" in
              if body <> "" && String.length body < 1024 then
                dispatch from body out
      | _ ->
          ()
            
let _ =
  Hooks.register_handle (Catch catch_1april)
    
