(*
 * (c) 2004-2012 Anastasia Gornostaeva
 *)

open JID
open Hooks
open Muc
open XMPPClient

open Sqlite3
module Sql = Markov_sql.Make(Sqlgg_sqlite3)

exception Result of string
  
module MarkovMap = Map.Make(GroupID)

let () = Random.self_init ()

type context = {
  dir : string;
  max_words : int;
  mutable markovrooms : m MarkovMap.t
}
and m = {
  queue: mevent Queue.t;
  mutex: Mutex.t;
  cond: Condition.t;
}
and mevent = 
  | MMessage of
      context * muc_context * xmpp * env * message_type option * JID.t *
        string * string
  | MStop 
  | MCount of context * xmpp * env * message_type option * JID.t
  | MTop of context * xmpp * env * message_type option * JID.t


let add_queue (m:m) (mevent:mevent) =
  Mutex.lock m.mutex;
  Queue.add mevent m.queue;
  Condition.signal m.cond;
  Mutex.unlock m.mutex
    
let take_queue (m:m) =
  Mutex.lock m.mutex;
  while Queue.is_empty m.queue do
    Condition.wait m.cond m.mutex;
  done;
  let e = Queue.take m.queue in
    Mutex.unlock m.mutex;
    e
      
let open_markovdb ctx (lnode, ldomain) =
  if not (Sys.file_exists ctx.dir) then Unix.mkdir ctx.dir 0o755;
  let file = Filename.concat ctx.dir (lnode ^ "@" ^ ldomain) in
  let db = Sqlite3.db_open file in
    ignore (Sql.create_words db);
    ignore (Sql.create_index_word1word2 db);
    db
        
let add db words =
  let rec aux_add w1 lst =
    match lst with
      | [] -> (
          match Sql.check_pair db ~word1:w1 ~word2:"" with
            | None -> ignore (Sql.add_pair db ~word1:w1 ~word2:"" ~counter:1L)
            | Some _ -> ignore (Sql.update_pair db ~word1:w1 ~word2:"")
        )
      | w2 :: tail ->
          if w1 = w2 then
            ()
          else (
            match Sql.check_pair db ~word1:w1 ~word2:w2 with
              | None -> ignore (Sql.add_pair db ~word1:w1 ~word2:w2 ~counter:1L)
              | Some _ -> ignore (Sql.update_pair db ~word1:w1 ~word2:w2)
          );
          aux_add w2 tail
  in
    aux_add "" words
        
let seek db w1 =
  let sum =
    match Sql.get_sum db ~word1:w1 with
      | None -> 0
      | Some c -> Int64.to_int c
  in
    if sum = 0 then
      ""
    else
      let callback word2 counter lsum =
        let i = Int64.to_int counter in
          if lsum - i <= 0 then
            raise (Result word2)
          else
            lsum - i
      in
        try
          let _ = Sql.Fold.get_pair db ~word1:w1 callback (Random.int sum + 1) in
            ""
        with Result result -> result
            
let generate ctx db word =
  let rec aux_chain w i acc =
    if i = ctx.max_words then
      let p = String.concat " " (List.rev acc) in
        p
    else
      let w2 = seek db w in
        if w2 = "" then String.concat " " (List.rev acc)
        else aux_chain w2 (i+1) (w2::acc)
  in
    aux_chain word 0 []
        
let split_words body =
  Pcre.split ~pat:"[ \t\n]+" body
    
let process_markov ctx db muc_context xmpp env kind jid_from nick text =
  let words = split_words text in
  let room_env = get_room_env muc_context jid_from in
    if words = [] then
      match kind with
        | Some Groupchat ->
            if nick = room_env.mynick then
              env.env_message xmpp kind jid_from "?"
        | Some Chat ->
            env.env_message xmpp kind jid_from "?"
        | _ ->
            ()
    else (
      add db words;
      match kind with
        | Some Groupchat ->
            if nick = room_env.mynick then
              let chain = generate ctx db "" in
                env.env_message xmpp kind jid_from chain
        | Some Chat ->
            let chain = generate ctx db "" in
              env.env_message xmpp kind jid_from chain
        | _ ->
            ()
    )        

let rec markov_thread (db, m) =
  (match take_queue m with
     | MMessage (ctx, muc_context, xmpp, env, kind, jid_from, nick, text) ->
         process_markov ctx db muc_context xmpp env kind jid_from nick text
     | MCount (ctx, xmpp, env, kind, jid_from) ->
         let result =
           match Sql.count db with
             | None -> 0
             | Some c -> Int64.to_int c
         in
           env.env_message xmpp kind jid_from (string_of_int result)
     | MTop (ctx, xmpp, env, kind, jid_from) ->
         let callback word1 word2 counter acc =
           let r =
             Printf.sprintf "\n%s | %s | %d" word1 word2 (Int64.to_int counter)
           in
             r :: acc
         in
         let top = Sql.Fold.get_top db callback [] in
           env.env_message xmpp kind jid_from (String.concat "" (List.rev top))
     | MStop -> 
         ignore (db_close db);
         Thread.exit ()
  );
  markov_thread (db, m)
    
let get_markov_queue ctx room =
  try
    MarkovMap.find room ctx.markovrooms
  with Not_found ->
    let db = open_markovdb ctx room in
    let m = {queue = Queue.create ();
             mutex = Mutex.create ();
             cond = Condition.create ()}
    in
      ignore (Thread.create markov_thread (db, m));
      ctx.markovrooms <- MarkovMap.add room m ctx.markovrooms;
      m
        
(*
let close_room jid_from =
  try
    let m = get_markov_queue ctx (jid_from.lnode, jid_from.ldomain) in
      add_queue m MStop;
      markovrooms := MarkovMap.remove (from.lnode, from.ldomain) 
        !markovrooms;
  with _ -> ()
*)
        
let markov_chain ctx muc_context xmpp env kind jid_from nick text =
  try
    let m = get_markov_queue ctx (jid_from.lnode, jid_from.ldomain) in
      add_queue m (MMessage (ctx, muc_context, xmpp, env, kind, jid_from, nick, text))
  with _ -> ()
        
let markov_count ctx xmpp env kind jid_from _text =
  (try
     let m = get_markov_queue ctx (jid_from.lnode, jid_from.ldomain) in
       add_queue m (MCount (ctx, xmpp, env, kind, jid_from))
   with _ -> ())
        
let markov_top ctx xmpp env kind jid_from _text =
  (try
     let m = get_markov_queue ctx (jid_from.lnode, jid_from.ldomain) in
       add_queue m (MTop (ctx, xmpp, env, kind, jid_from))
   with _ -> ())

let plugin opts =
  let dir =
    try List.assoc "path" (List.assoc "dir" opts)
    with Not_found -> "./markov_db" in
  let max_words = get_int opts "max_words" "value" 20 in
    Muc.add_for_muc_context
      (fun muc_context xmpp ->
         let ctx = {
           dir = dir;
           max_words = max_words;
           markovrooms = MarkovMap.empty
         } in
           Muc.add_hook_conversation muc_context (markov_chain ctx);
           Plugin_command.add_commands xmpp
             [("markov_count", markov_count ctx);
              ("markov_top", markov_top ctx)] opts
      )
    
let () =
  Plugin.add_plugin "markov" plugin
