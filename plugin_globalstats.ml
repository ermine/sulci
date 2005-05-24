(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Common
open Xmpp
open Xml
open Error
open Types

let stats_sum serverlist result out () =
   let totals = ref 0 in
   let onlines = ref 0 in
   let servers = ref 0 in
   let sin = open_in serverlist in
   let rec each_server server =
      let proc e f x o =
	 (match e with
	     | Iq `Result ->
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
	    let sout = open_out result in
	       output_string sout (Printf.sprintf "%d\n%d\n%d\n"
				      !totals !onlines !servers);
	       close_in sin;
	       close_out sout
      in
      let id = new_id () in
	 Hooks.register_handle (Hooks.Id (id, proc));
	 out (Xmlelement 
		 ("iq", ["to", server; "type", "get"; "id", id],
		  [Xmlelement 
		      ("query", ["xmlns", 
				 "http://jabber.org/protocol/stats"],
		       [Xmlelement ("stat", ["name", "users/online"], []);
			Xmlelement ("stat", ["name", "users/total"], [])
		       ])]))
   in
   let server = input_line sin in
      each_server server

let cmd_stats text event from xml out =
   let server = text in
   let proc e f x o =
      match e with
	 | Iq `Result ->
	      let stats = get_subels ~path:["query"] ~tag:"stat" x in
	      let data = List.map (fun z ->
				      get_attr_s z "name",		 
				      try 
					 get_attr_s z "value"
				      with Not_found -> "unknown" ) stats in
		 o (make_msg xml 
		       (Printf.sprintf "\nUsers Total: %s\nUsers Online: %s"
			   (List.assoc "users/total" data)
			   (List.assoc "users/online" data)))
	 | Iq `Error ->
	      o (make_msg xml 
		    (Lang.get_msg ~xml "plugin_globalstats_stats_error" []))
	 | _ -> ()
   in
   let id = new_id () in
      Hooks.register_handle (Hooks.Id (id, proc));
      out (Xmlelement 
	      ("iq", ["to", server; "type", "get"; "id", id],
	       [Xmlelement ("query", ["xmlns", 
				      "http://jabber.org/protocol/stats"],
			    [Xmlelement ("stat", ["name", "users/online"], []);
			     Xmlelement ("stat", ["name", "users/total"], [])
			    ])]))

let uptime text event from xml out =
   if text = "" then 
      out (make_msg xml 
	      (Lang.get_msg ~xml "plugin_globalstats_uptime_invalid_syntax" []))
   else
      let proc e f x o =
	 match e with
	    | Iq `Result ->
		 let seconds = get_attr_s x ~path:["query"] "seconds" in
		 let last = 
		    Lang.expand_time ~xml "uptime" (int_of_string seconds) in
		    o (make_msg xml
			  (Lang.get_msg ~xml "plugin_globalstats_uptime"
			      [text; last]))
	    | Iq `Error ->
		 let reply =
		    let cond, type_, text = parse_error x in
		       match cond with
			  | `ERR_FEATURE_NOT_IMPLEMENTED ->
			       Lang.get_msg ~xml
				  "plugin_globalstats_uptime_not_implemented" 
				  []
			  | `ERR_REMOTE_SERVER_TIMEOUT ->
			       "remote server timeout"
			  | `ERR_REMOTE_SERVER_NOT_FOUND ->
			       Lang.get_msg ~xml 
			     "plugin_globalstats_uptime_remote_server_not_found"
				  []
			  | _ ->
			       Lang.get_msg ~xml 
				  "plugin_globalstats_uptime_error" []
		 in
		    o (make_msg xml reply)
	    | _ -> ()
      in
      let id = new_id () in
	 Hooks.register_handle (Hooks.Id (id, proc));
	 out (iq_query ~to_:text ~id "jabber:iq:last")

let _ =
   if Xml.mem_xml Config.config ["sulci"; "plugins"; "globalstats"] "store" [] 
   then
      begin
	 let serverlist = get_attr_s Config.config 
	    ~path:["plugins"; "globalstats"; "store"] "serverlist" in
	 let result = get_attr_s Config.config 
	    ~path:["plugins"; "globalstats"; "store"] "result" in
	 let interval = float_of_string 
	    (get_attr_s Config.config
		~path:["plugins"; "globalstats"; "store"] "interval") in
	    
	 let start_stats out =
	    (* let _ = Timer.register (stats_sum serverlist result out)
	       (interval *. 1000.) 86400000. *)
	    let _ = Scheduler.add_task (stats_sum serverlist result out)
	       (Unix.gettimeofday () +. 10.) interval
	    in ()
	 in
	    Hooks.register_handle (Hooks.OnStart start_stats)
      end;
   Hooks.register_handle (Hooks.Command ("stats", cmd_stats));
   Hooks.register_handle (Hooks.Command ("uptime", uptime))
