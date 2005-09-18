(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Common
open Unix
open Pcre
open Muc
open Types
open Xmpp

let basedir = 
   let dir = trim (Xml.get_cdata Config.config ~path:["muc"; "chatlogs"]) in
      if not (Sys.file_exists dir) then mkdir dir 0o755;
      dir

module LogMap = Map.Make(GID)
let logmap = ref LogMap.empty

let open_log (user, host) =
   let tm = localtime (time ()) in
   let year = tm.tm_year + 1900 in
   let month = tm.tm_mon + 1 in
   let day = tm.tm_mday in

   let p1 = Filename.concat basedir (user ^ "@" ^ host) in
   let () = if not (Sys.file_exists p1) then mkdir p1 0o755 in
   let p2 = Printf.sprintf "%s/%i" p1 year in
   let () = if not (Sys.file_exists p2) then mkdir p2 0o755 in
   let p3 = Printf.sprintf "%s/%0.2i" p2 month in
   let () = if not (Sys.file_exists p3) then mkdir p3 0o755 in
   let file = Printf.sprintf "%s/%0.2i.html" p3 day in
      if not (Sys.file_exists file) then
	 let out_log = open_out_gen [Open_creat; Open_append] 0o644 file in
	    output_string out_log 
	       (Printf.sprintf 
		   "<html><head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />
<title>%s@%s - %0.2d/%0.2d/%d</title></head>\n
<body><h1>%s@%s - %0.2d/%0.2d/%d</h1>\n"
		   user host day month year user host day month year);
	    flush out_log;
	    out_log
      else
	 open_out_gen [Open_append] 0o644 file


let get_next_noun () =
   let curr_time = gettimeofday () in
   let curr_tm = localtime curr_time in
   let noun, _ = mktime {curr_tm with 
			    tm_sec = 0; tm_min = 0; tm_hour = 0;
			    tm_mday = curr_tm.tm_mday + 1} in
      noun

let rotate_logs () =
   logmap := LogMap.mapi (fun room lf ->
			     output_string lf "</body>\n</html>";
			     flush lf;
			     close_out lf;
			     open_log room) !logmap
   
let _ = Scheduler.add_task rotate_logs (get_next_noun ()) (fun () -> 86400.)

let get_logfile room =
   try 
      LogMap.find room !logmap 
   with Not_found -> 
      let out_log = open_log room in
	 logmap := LogMap.add room out_log !logmap;
	 out_log

(*
let rex = regexp ~flags:[`CASELESS;]
   "((https?|ftp)://.*(?![?!,.]*(\\s|$))[^\\s])|((www|ftp)[a-z0-9.-]*\\.[a-z]{2,}.*(?![?!,.]*(\\s|$))[^\\s])"

let html_url text =
   try
      substitute ~rex 
	 ~subst:(fun url ->
		    if pmatch ~pat:".+//:" url then
		       Printf.sprintf "<a href='%s'>%s</a>" url url
		    else if pmatch ~pat:"^www" url then
		       Printf.sprintf "<a href='http://%s'>%s</a>" url url
		    else if pmatch ~pat:"^ftp" url then
		       Printf.sprintf "<a href='ftp://%s'>%s</a>" url url
		    else 
		       Printf.sprintf "<a href='%s'>%s</a>" url url
		)
	 text
   with Not_found -> text
*)

open Find_url

let html_url text =
   find_url make_hyperlink text

let make_message author nick body =
   let text =
      Pcre.substitute_substrings ~pat:"\n( *)"
	 ~subst:(fun s ->
		    try
		       let sub = Pcre.get_substring s 1 in
		       let len = String.length sub in
		       let buf = Buffer.create (len * 6) in
			  for i = 1 to len do
			     Buffer.add_string buf "&nbsp;"
			  done;
			  "<br>\n" ^ Buffer.contents buf
		    with _ -> "<br>"
		) body
   in
      if nick <> "" then
	 Printf.sprintf "&lt;%s&gt; %s: %s" author nick  (html_url text)
      else
	 Printf.sprintf "&lt;%s&gt; %s" author (html_url text)

let write (room:string * string) text =
   if text <> "" then
      let out_log = get_logfile room in
      let curtime = Strftime.strftime ~tm:(localtime (time ())) "%H:%M" in
	 output_string out_log 
	    (Printf.sprintf 
		"[%s] %s<br>\n"
		curtime text);
	 flush out_log

let process_log event (from:jid) xml =
   let lang room = (GroupchatMap.find room !groupchats).lang in
      match event with
	 | MUC_history -> ()
	 | MUC_topic subject ->
	      if from.resource <> "" then
		 write (from.luser, from.lserver) 
		    (Lang.get_msg ~lang:(lang (from.luser, from.lserver)) 
			"muc_log_set_subject" 
			[from.resource;  html_url subject])
	      else
		 write (from.luser, from.lserver) 
		    (Lang.get_msg ~lang:(lang (from.luser, from.lserver))
			"muc_log_subject" [html_url subject])
	 | MUC_message (msg_type, nick, body) when msg_type = `Groupchat  ->
	      if body <> "" then
		 write (from.luser, from.lserver) (
		    if nick = "" then
		       if String.length body = 3 && body = "/me" then
			  Printf.sprintf "* %s" from.resource
		       else if String.length body > 3 && 
			  String.sub body 0 4 = "/me " then
			     Printf.sprintf "* %s %s" from.resource
				(html_url (string_after body 4))
		       else
			  make_message from.resource nick body
		    else
		       make_message from.resource nick body)
	 | MUC_join item ->
	      write (from.luser, from.lserver)
		 ("-- " ^ Lang.get_msg ~lang:(lang (from.luser, from.lserver)) 
		     "muc_log_join" 
		     [from.resource])
	 | MUC_leave (reason, item) ->
	      write (from.luser, from.lserver)
		 ("-- " ^
		     (if reason = "" then
			 Lang.get_msg ~lang:(lang (from.luser, from.lserver))
			    "muc_log_leave" 
			    [from.resource]
		      else
			 Lang.get_msg ~lang:(lang (from.luser, from.lserver))
			    "muc_log_leave_reason" 
			    [from.resource; reason]))
	 | MUC_kick (reason,item) ->
	      write (from.luser, from.lserver)
		 ("-- " ^
		     (if reason = "" then
			 Lang.get_msg ~lang:(lang (from.luser, from.lserver))
			    "muc_log_kick" 
			    [from.resource]
		      else
			 Lang.get_msg ~lang:(lang (from.luser, from.lserver))
			    "muc_log_kick_reason" 
			    [from.resource; reason]))
	 | MUC_ban (reason, item) ->
	      write (from.luser, from.lserver)
		 ("-- " ^
		     (if reason = "" then
			 Lang.get_msg ~lang:(lang (from.luser, from.lserver))
			    "muc_log_ban" 
			    [from.resource]
		      else
			 Lang.get_msg ~lang:(lang (from.luser, from.lserver))
			    "muc_log_ban_reason" [from.resource; reason]))
	 | MUC_change_nick (newnick, item) ->
	      write (from.luser, from.lserver)
		 ("-- " ^
		     (Lang.get_msg ~lang:(lang (from.luser, from.lserver)) 
			 "muc_log_change_nick" [from.resource; newnick]))
(*
	 | MUC_presence (room, user, item) ->
	   (* Lang.get_msg ~lang:(lang room) "muc_log_presence" 
	      [user; item.show; item.status] *)
	      write room
		 (Printf.sprintf "-- %s [%s] %s" user item.show item.status)
*)
	 | _ -> ()

(*
let _ =
   Hooks.register_handle (Filter process_log
*)
