(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open XMPP
open Xep_stats
open Config
open Common
open Hooks

let find name alist =
  try
    let t = List.find (fun t -> t.name = name) alist in
      int_of_string t.value
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
                   let stats = decode el in
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
      XMPP.make_iq_request xmpp ~jid_to:(Jid.make_jid "" server "")
        (IQGet (make_iq_get ["users/total"; "users/online"])) proc
  in
  let server = input_line sin in
    each_server server
      
let _ =
  if Light_xml.mem_xml Config.config
    ["sulci"; "plugins"; "globalstats"] "store" [] then (
    let serverlist = Light_xml.get_attr_s Config.config 
      ~path:["plugins"; "globalstats"; "store"] "serverlist" in
    let result = Light_xml.get_attr_s Config.config 
      ~path:["plugins"; "globalstats"; "store"] "result" in
    let interval = float_of_string 
      (Light_xml.get_attr_s Config.config
         ~path:["plugins"; "globalstats"; "store"] "interval") in
      
    let start_stats out =
      let _ = Scheduler.add_task timerQ
        (fun () -> try stats_sum serverlist result out with exn ->
           log#error "Plugin_globalstats: %s" (Printexc.to_string exn))
        (Unix.gettimeofday () +. 10.) (fun () -> interval)
      in ()
    in
      register_on_connect start_stats
  )
