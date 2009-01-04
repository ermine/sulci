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
open Muc
open Sqlite3
open Sqlite_util

let table = "words"

type mevent = 
  | MMessage of muc_event * jid * Xml.element * local_env * (Xml.element -> unit) 
  | MStop 
  | MCount of jid * Xml.element * local_env * (Xml.element -> unit)
  | MTop of jid * Xml.element * local_env * (Xml.element -> unit)

type t = {
  queue: mevent Queue.t;
  mutex: Mutex.t;
  cond: Condition.t;
}

module MarkovMap = Map.Make(GID)
let markovrooms = ref MarkovMap.empty

let _ = Random.self_init ()

let add_queue (m:t) (mevent:mevent) =
  Mutex.lock m.mutex;
  Queue.add mevent m.queue;
  Condition.signal m.cond;
  Mutex.unlock m.mutex
    
let take_queue (m:t) =
  Mutex.lock m.mutex;
  while Queue.is_empty m.queue do
    Condition.wait m.cond m.mutex;
  done;
  let e = Queue.take m.queue in
    Mutex.unlock m.mutex;
    e
      
let open_markovdb (lnode, ldomain) =
  let path = 
    try trim (Xml.get_attr_s Config.config 
                ~path:["plugins"; "markov"] "dir")
    with Not_found -> "./markov_db"
  in
    if not (Sys.file_exists path) then Unix.mkdir path 0o755;
    let file = Filename.concat path (lnode ^ "@" ^ ldomain) in
    let db = Sqlite3.db_open file in
      create_table file db 
        (Printf.sprintf
           "SELECT name FROM SQLITE_MASTER WHERE type='table' AND name='%s'"
           table)
        (Printf.sprintf 
           "CREATE TABLE %s (word1 varchar(256), word2 varchar(256), counter int);
        CREATE INDEX word1word2 ON %s (word1, word2)"
           table table);
      file, db
        
let add file db words =
  let rec cycle1 w1 lst =
    match lst with
      | [] -> (
          let sql1 = Printf.sprintf
            "SELECT counter FROM %s WHERE word1=%s AND word2=''"
            table (escape w1) in
          let sql2 =
            Printf.sprintf
              "UPDATE %s SET counter=counter+1 WHERE word1=%s AND word2=''"
              table (escape w1) in
          let sql3 = 
            Printf.sprintf
              "INSERT INTO %s (word1, word2, counter) VALUES(%s, '', 1)"
              table ( escape w1)
          in
            ignore (insert_or_update file db sql1 sql2 sql3)
        )
      | w2 :: tail -> (
          if w1 = w2 then
            cycle1 w2 tail
          else (
            let sql1 = Printf.sprintf
              "SELECT counter FROM %s WHERE word1=%s AND word2=%s"
              table (escape w1) (escape w2) in
            let sql2 = Printf.sprintf
              "UPDATE %s SET counter=counter+1 WHERE word1=%s AND word2=%s"
              table (escape w1) (escape w2) in
            let sql3 = Printf.sprintf
              "INSERT INTO %s (word1, word2, counter) VALUES(%s, %s, 1)"
              table (escape w1) (escape w2) in
              ignore (insert_or_update file db sql1 sql2 sql3);
              cycle1 w2 tail
          )
        )
  in
    try
      cycle1 "" words
    with exn ->
      log#error "Plugin_markov %s" (Printexc.to_string exn)
        
let seek file db (w1:string) =
  let sum =
    match get_one_row file db
      (Printf.sprintf "SELECT sum(counter) FROM %s WHERE word1=%s"
         table (escape w1)) with
        | None -> 0
        | Some r -> Int64.to_int (int64_of_data r.(0))
  in
    if sum = 0 then
      w1, ""
    else
      let sql = Printf.sprintf 
        "SELECT word1, word2, counter FROM %s WHERE word1=%s"
        table (escape w1) in
      let rec aux_seek lsum stmt =
        match step stmt with
          | Rc.ROW ->
              let i = int_of_string (Data.to_string (column stmt 2)) in
                if lsum - i <= 0 then
                  (Data.to_string (column stmt 0),
                   Data.to_string (column stmt 1))
                else
                  aux_seek (lsum - i) stmt
          | Rc.DONE ->
              w1, ""
          | _ -> exit_with_rc file db sql
      in        
        try
          let stmt = prepare db sql in
          let w1, w2 = aux_seek (Random.int sum + 1) stmt in
            if finalize stmt <> Rc.OK then
              exit_with_rc file db sql;
            w1, w2
        with Sqlite3.Error _ ->
          exit_with_rc file db sql
            
let chain_limit = ref
  (try int_of_string (get_attr_s Config.config ~path:["plugin"; "markov"]
                        "msg_limit")
   with Not_found -> 20)
  
let generate file db word =
  let rec cycle3 w i acc =
    if i = !chain_limit then
      let p = String.concat " " (List.rev acc) in
        p
    else
      let w1, w2 = seek file db w in
        if w2 = "" then String.concat " " (List.rev acc)
        else cycle3 w2 (i+1) (w2::acc)
  in
    try
      cycle3 word 0 []
    with exn ->
      log#error "Plugin_markov: generate a phrase: %s" (Printexc.to_string exn);
      ""
        
let split_words body =
  Pcre.split ~pat:"[ \t\n]+" body
    
let process_markov file db event from xml env out =
  match event with
    | MUC_message (msg_type, nick, body) ->
        let room_env = get_room_env from in
          if from.lresource <> room_env.mynick then
            let words = split_words body in
              if words = [] then (
                if (msg_type = `Groupchat && nick = room_env.mynick) ||
                  msg_type <> `Groupchat then
                    make_msg out xml "?"
              ) else (
                add file db words;
                if (msg_type = `Groupchat && nick = room_env.mynick) ||
                  msg_type <> `Groupchat then
                    let chain = generate file db "" in
                      make_msg out xml chain
                else
                  ()
              )
          else
            ()
    | _ -> ()
          
let rec markov_thread (file, db, m) =
  (match take_queue m with
     | MMessage (event, from, xml, env, out) ->
         process_markov file db event from xml env out
     | MCount (from, xml, env, out) ->
         let sql = Printf.sprintf "SELECT COUNT(*) FROM %s" table in
         let result =
           match get_one_row file db sql with
             | None -> 9
             | Some r -> Int64.to_int (int64_of_data r.(0))
         in
           make_msg out xml (string_of_int result)
     | MTop (from, xml, env, out) -> (
         let sql = Printf.sprintf
           "SELECT word1, word2, counter FROM %s WHERE word1!='' AND word2!='' ORDER BY counter DESC LIMIT 10" table in
         let rec aux_top acc stmt =
           match step stmt with
             | Rc.ROW ->
                 let r =
                   Printf.sprintf "\n%s | %s | %s"
                     (Data.to_string (column stmt 0))
                     (Data.to_string (column stmt 1))
                     (Data.to_string (column stmt 2)) in
                   aux_top (r::acc) stmt
             | Rc.DONE ->
                 List.rev acc
             | _ -> exit_with_rc file db sql
         in
           try
             let stmt = prepare db sql in
             let data = aux_top [] stmt in
               make_msg out xml (String.concat "" data)
           with
             | Sqlite3.Error _ ->
                 exit_with_rc file db sql
             | exn ->
                 log#error "Plugin_markov: !!!top: %s" (Printexc.to_string exn)
       )
     | MStop -> 
         db_close db;
         Thread.exit ()
  );
  markov_thread (file, db, m)
    
let get_markov_queue room =
  try
    MarkovMap.find room !markovrooms
  with Not_found ->
    let file, db = open_markovdb room in
    let m = {queue = Queue.create (); mutex = Mutex.create ();
             cond = Condition.create ()} in
      ignore (Thread.create markov_thread (file, db, m));
      markovrooms := MarkovMap.add room m !markovrooms;
      m
        
let markov_chain event from xml env out =
  match event with
    | MUC_message _ ->
        (try
           let m = get_markov_queue (from.lnode, from.ldomain) in
             add_queue m (MMessage (event, from, xml, env, out))
         with _ -> ())
    | MUC_leave (true, _, _, _) ->
        (try
           let m = get_markov_queue (from.lnode, from.ldomain) in
             add_queue m MStop;
             markovrooms := MarkovMap.remove (from.lnode, from.ldomain) 
               !markovrooms;
         with _ -> ())
    | _ -> ()
        
let markov_count text from xml env out =
  (try
     let m = get_markov_queue (from.lnode, from.ldomain) in
       add_queue m (MCount (from, xml, env, out))
   with _ -> ())
        
let markov_top text from xml env out =
  (try
     let m = get_markov_queue (from.lnode, from.ldomain) in
       add_queue m (MTop (from, xml, env, out))
   with _ -> ())
        
let _ =
  register_catcher markov_chain;
  register_command "!!!count", markov_count;
  register_command "!!!top", markov_top
    
