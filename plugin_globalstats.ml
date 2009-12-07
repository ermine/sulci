(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Unix
open XMPP
open Hooks
open Plugin_scheduler

module S = XEP_stats

let find name alist =
  try
    let t = List.find (fun t -> t.S.name = name) alist in
      int_of_string t.S.value
  with _ -> 0
    
let stats_sum serverlist result xmpp =
  log#info "Globalstats: Start polling";
  let totals = ref 0 in
  let onlines = ref 0 in
  let servers = ref 0 in
  let sin = open_in serverlist in
  let rec each_server server =
    let proc ev jid_from jid_to lang () =
      (match ev with
         | IQResult el -> (
             match el with
               | None -> ()
               | Some el ->
                   let stats = S.decode el in
                   let total = find "users/total" stats in
                   let online = find "users/online" stats in
                     totals := !totals + total;
                     onlines := !onlines + online;
                     servers := !servers + 1
           )
         | IQError _err -> ());
      try
        let server = input_line sin in
          each_server server
      with End_of_file ->
        log#error
          "Globalstats: end polling, results: %d total, %d onlines, %d servers" 
          !totals !onlines !servers;
        let sout = open_out result in
          output_string sout (Printf.sprintf "%d\n%d\n%d\n"
                                !totals !onlines !servers);
          close_in sin;
          close_out sout
    in
      XMPP.make_iq_request xmpp ~jid_to:(JID.make_jid "" server "")
        (IQGet (S.make_iq_get ["users/total"; "users/online"])) proc
  in
  let server = input_line sin in
    each_server server
      
let poll serverlist result xmpp () =
  try stats_sum serverlist result xmpp with exn ->
    log#error "Plugin_globalstats: %s" (Printexc.to_string exn)

let get_next interval () =
  let curr_tm = localtime (gettimeofday ()) in
  let next, _ = mktime {curr_tm with tm_min = curr_tm.tm_min + interval} in
    next

let plugin opts =
  let serverlist = List.assoc "file" (List.assoc "serverlist" opts) in
  let result = List.assoc "file" (List.assoc "result" opts) in
  let interval =
    let v = List.assoc "value" (List.assoc "interval" opts) in
      int_of_string v
    in
      add_for_token
        (fun _opts xmpp ->
           let _ = Scheduler.add_task timerQ (poll serverlist result xmpp)
             (get_next interval ()) (get_next interval) in
             ()
        )

let () =
  Plugin.add_plugin "globalstats" plugin
