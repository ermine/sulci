(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Common
open Unix
open Pcre
open Hooks
open Muc

let _ = Scheduler.init ()


let basedir = 
   let dir = trim (Xml.get_cdata Config.config ~path:["muc"; "chatlogs"]) in
      if not (Sys.file_exists dir) then mkdir dir 0o755;
      dir

module LogMap = Map.Make(Id)
let logmap = ref LogMap.empty


let open_log room =
   let tm = localtime (time ()) in
   let year = tm.tm_year + 1900 in
   let month = tm.tm_mon + 1 in
   let day = tm.tm_mday in

   let p1 = Filename.concat basedir room in
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
<title>%s - %0.2d/%0.2d/%d</title></head>\n
<body><h1>%s - %0.2d/%0.2d/%d</h1>\n"
		   room day month year room day month year);
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
     (* noun -. curr_time *)
      noun

let rec rotate_logs () =
   logmap := LogMap.mapi (fun room lf ->
			     output_string lf "</body>\n</html>";
			     flush lf;
			     close_out lf;
			     open_log room) !logmap
   
(* let _ = Timer.register rotate_logs ((get_next_noun ()) *. 1000.) 86400000. *)
let _ = Scheduler.add_task rotate_logs (get_next_noun ()) 86400000.

let get_logfile room =
   try 
      LogMap.find room !logmap 
   with Not_found -> 
      let out_log = open_log room in
	 logmap := LogMap.add room out_log !logmap;
	 out_log

let rex = regexp "((https?|ftp)://[^ ]+|(www|ftp)[a-z0-9.-]*\\.[a-z]{2,4}[^ ]*)"

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

let make_message nick body =
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
      Printf.sprintf "&lt;%s&gt; %s" nick (html_url text)

let process_log room event xml out =
   let lang = (GroupchatMap.find room !groupchats).lang in
   let text = match event with
      | MUC_history -> ""
      | MUC_topic (nick, subject) ->
	   Lang.get_msg ~lang "muc_log_set_subject" [nick;  html_url subject]
      | MUC_message (nick, body) ->
	   if body <> "" then
	      if pmatch ~pat:"/me" body then
		 let action = string_after body 4 in
		    Printf.sprintf "* %s %s" nick  (html_url action)
	      else
		 make_message nick body
	   else
	      ""
      | MUC_join (user, item) ->
	   "--" ^ Lang.get_msg ~lang "muc_log_join" [user]
      | MUC_leave (user, reason, item) ->
	   if reason = "" then
	      Lang.get_msg ~lang "muc_log_leave" [user]
	   else
	      Lang.get_msg ~lang "muc_log_leave_reason" [user; reason]
      | MUC_kick (user, reason,item) ->
	   if reason = "" then
	      Lang.get_msg ~lang "muc_log_kick" [user]
	   else
	      Lang.get_msg ~lang "muc_log_kick_reason" [user; reason]
      | MUC_ban (user, reason, item) ->
	   if reason = "" then
	      Lang.get_msg ~lang "muc_log_ban" [user]
	   else
	      Lang.get_msg ~lang "muc_log_ban_reason" [user; reason]
      | MUC_change_nick (newnick, user, item) ->
	   Lang.get_msg ~lang "muc_log_change_nick" [user; newnick]
      | MUC_presence (user, item) ->
	   (* Lang.get_msg ~lang "muc_log_presence" 
	      [user; item.show; item.status] *)
	   Printf.sprintf "%s [%s] %s" user item.show item.status
      | _ -> ""
   in
      if text <> "" then
	 let out_log = get_logfile room in
	 let curtime = Strftime.strftime ~tm:(localtime (time ())) "%H:%M" in
	    output_string out_log 
	       (Printf.sprintf 
		   "[%s] %s<br>\n"
		   curtime text);
	    flush out_log

let _ =
   Muc.register_handle process_log
