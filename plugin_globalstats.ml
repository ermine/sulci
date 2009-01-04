(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open Xmpp
open Error
open Types
open Config
open Common

let stats_sum serverlist result out =
  log#info "Globalstats: Start polling";
  let totals = ref 0 in
  let onlines = ref 0 in
  let servers = ref 0 in
  let sin = open_in serverlist in
  let rec each_server server =
    let proc t f x o =
      (match t with
         | `Result ->
             let stats = get_subels ~path:["query"] ~tag:"stat" x in
             let data = List.map (fun z ->
                                    get_attr_s z "name",
                                    try int_of_string 
                                      (get_attr_s z "value")
                                    with Not_found -> 0 ) stats in
               totals := !totals + List.assoc "users/total" data;
               onlines := !onlines + List.assoc "users/online" data;
               servers := !servers + 1
         | _ -> ());
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
    let id = new_id () in
      Hooks.register_iq_query_callback id proc;
      out (make_element "iq" ["to", server; "type", "get"; "id", id]
             [make_element "query" ["xmlns", "http://jabber.org/protocol/stats"]
                [make_element "stat" ["name", "users/online"] [];
                 make_element "stat" ["name", "users/total"] []]])
  in
  let server = input_line sin in
    each_server server
      
let _ =
  if Xml.mem_xml Config.config ["sulci"; "plugins"; "globalstats"] "store" [] 
  then (
    let serverlist = get_attr_s Config.config 
      ~path:["plugins"; "globalstats"; "store"] "serverlist" in
    let result = get_attr_s Config.config 
      ~path:["plugins"; "globalstats"; "store"] "result" in
    let interval = float_of_string 
      (get_attr_s Config.config
         ~path:["plugins"; "globalstats"; "store"] "interval") in
      
    let start_stats out =
      let _ = Scheduler.add_task Types.timerQ
        (fun () -> try stats_sum serverlist result out with exn ->
           log#error "Plugin_globalstats: %s" (Printexc.to_string exn))
        (Unix.gettimeofday () +. 10.) (fun () -> interval)
      in ()
    in
      Hooks.register_on_connect start_stats
  )
