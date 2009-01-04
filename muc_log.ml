(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Unix
open Pcre
open Jid
open Types
open Config
open Common
open Muc_types

let basedir = 
  let dir = trim (Xml.get_cdata Config.config ~path:["muc"; "chatlogs"]) in
    if not (Sys.file_exists dir) then mkdir dir 0o755;
    dir

module LogMap = Map.Make(GID)
let logmap = ref LogMap.empty

let open_log (user, host) =
  let tm = localtime (gettimeofday ()) in
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
<meta name='all' content='nofollow' />
<title>%s@%s - %0.2d/%0.2d/%d</title></head>\n
<body><h1>%s@%s - %0.2d/%0.2d/%d</h1>\n"
             user host day month year user host day month year);
        flush out_log;
        out_log
    else
      open_out_gen [Open_append] 0o644 file

let get_next_noun () =
  let curr_tm = localtime (gettimeofday ()) in
  let noun, _ = mktime
    {curr_tm with 
       tm_sec = 0; tm_min = 0; tm_hour = 0; tm_mday = curr_tm.tm_mday + 1} in
    noun

let rotate_logs () =
  log#info "MUC Log: Rotating chatlogs";
  logmap :=
    LogMap.mapi (fun room lf ->
                   let old = lf in
                   let newlog = open_log room in
                     output_string old "</body>\n</html>";
                     flush old;
                     close_out old;
                     newlog) !logmap
    
let _ =
  Scheduler.add_task Types.timerQ rotate_logs (get_next_noun ()) get_next_noun
  
let add_chatlog room_jid =
  let room = room_jid.lnode, room_jid.ldomain in
  let out_log = open_log room in
    logmap := LogMap.add room out_log !logmap

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

let make_message author body =
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
    Printf.sprintf "&lt;%s&gt; %s" author (html_url text)

let write (room:string * string) text =
  if text <> "" then
    let out_log = LogMap.find room !logmap in
    let curtime = 
      Strftime.strftime ~tm:(localtime (gettimeofday ())) "%H:%M" in
      output_string out_log 
        (Printf.sprintf 
           "[%s] %s<br>\n"
           curtime text);
      flush out_log
        
let process_log event (from:jid) xml env =
  let room = from.lnode, from.ldomain in
    if LogMap.mem room !logmap then
      match event with
        | MUC_history -> ()
        | MUC_topic subject ->
            if from.resource <> "" then
              write room 
                (Lang.get_msg env.env_lang "muc_log_set_subject" 
                   [from.resource;  html_url subject])
            else
              write room 
                (Lang.get_msg env.env_lang "muc_log_subject" [html_url subject])
        | MUC_message (msg_type, _, _) when msg_type = `Groupchat  ->
            let body = Xml.get_cdata xml ~path:["body"] in
              if body <> "" then
                write room (
                  if String.length body = 3 && body = "/me" then
                    Printf.sprintf "* %s" from.resource
                  else if String.length body > 3 && 
                    String.sub body 0 4 = "/me " then
                      Printf.sprintf "* %s %s" from.resource
                        (html_url (string_after body 4))
                  else
                    make_message from.resource body)
        | MUC_join item ->
            write room
              ("-- " ^ Lang.get_msg env.env_lang "muc_log_join" 
                 [from.resource])
        | MUC_leave (me, t, reason, item) ->
            write room
              ("-- " ^
                 (if reason = "" then
                    Lang.get_msg env.env_lang
                      (match t with
                         | `Kick -> "muc_log_kick"
                         | `Ban -> "muc_log_ban"
                         | `UnMember -> "muc_log_unmember"
                         | `Normal -> "muc_log_leave")
                      [from.resource]
                  else
                    Lang.get_msg env.env_lang
                      (match t with
                         | `Kick -> "muc_log_kick_reason"
                         | `Ban -> "muc_log_ban_reason"
                         | `UnMember -> "muc_log_unmember_reason"
                         | `Normal -> "muc_log_leave_reason")
                      [from.resource; html_url reason]));
            if me then
              let lf = LogMap.find room !logmap in
                output_string lf "</body>\n</html>";
                flush lf;
                close_out lf;
                logmap := LogMap.remove room !logmap
                  
        | MUC_change_nick (newnick, item) ->
            write room
              ("-- " ^
                 (Lang.get_msg env.env_lang 
                    "muc_log_change_nick" [from.resource; newnick]))
              (*
                | MUC_presence (room, user, item) ->
              (* Lang.get_msg env.env_lang "muc_log_presence" 
                [user; item.show; item.status] *)
                write room
              (Printf.sprintf "-- %s [%s] %s" user item.show 
                html_url item.status)
              *)
        | _ -> ()
            
