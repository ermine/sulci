(*
 * (c) 2004-2010 Anastasia Gornostaeva
 *)

open XMPP
open JID
open Hooks
open Muc

exception Result of string

module Index = Map.Make(String)

let () = Random.self_init ()

let open_markovdb path (lnode, ldomain) =
  if not (Sys.file_exists path) then Unix.mkdir path 0o755;
  let file = Filename.concat path (lnode ^ "@" ^ ldomain) in
  let db = Dbm.opendbm file [Dbm.Dbm_rdwr;Dbm.Dbm_create] 0o644 in
    file, db

type group = (int * string) array

let to_str (x:group) = Marshal.to_string x []
let of_str s : group = Marshal.from_string s 0

let dbm_find t k = try Some (of_str (Dbm.find t k)) with Not_found -> None

let seek file db w1 =
  match dbm_find db w1 with
    | None -> ""
    | Some g -> 
        let sum = Array.fold_left (fun acc (n,_) -> acc + n) 0 g in
        let lsum = ref (Random.int sum + 1) in
          try
            Array.iter (fun (cnt,w2) ->
                          lsum := !lsum - cnt;
                          if !lsum <= 0 then raise (Result w2)) g;
            ""
          with Result w2 -> w2

let add file db words =
  let update w1 w2 =
    if w1 = w2 then ()
    else
      begin
        match dbm_find db w1 with
          | None -> Dbm.add db w1 (to_str [|1,w2|])
          | Some g ->
              try
                let i = Array.findi (fun (_,w) -> w = w2) g in
                let (n,w) = g.(i) in
                  g.(i) <- (n+1,w);
                  Dbm.replace db w1 (to_str g)
              with
                  Not_found -> Dbm.replace db w1 (to_str (Array.append g [|1,w2|]))
      end
  in
  let rec cycle1 w1 = function
    | [] -> update w1 ""
    | w2 :: tail -> update w1 w2; cycle1 w2 tail
  in
    try
      cycle1 "" words
    with exn ->
      Printf.printf "Plugin_markov %s\n%!" (Printexc.to_string exn)

let chain_limit = ref 20

let generate file db =
  let rec cycle3 w i acc =
    if i = !chain_limit then
      String.concat " " (List.rev acc)
    else
      match seek file db w with
        | "" -> String.concat " " (List.rev acc)
        | w2 -> cycle3 w2 (i+1) (w2::acc)
  in
    try
      cycle3 "" 0 []
    with exn ->
      Printf.printf "Plugin_markov: generate a phrase: %s\n%!"
        (Printexc.to_string exn);
      ""
        
let split_words body =
  Pcre.split ~pat:"[ \t\n]+" body

  

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
  
