(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Unix
open Pcre
open XMPP
open Jid
open Hooks
open Muc
open Plugin_scheduler
  
module LogMap = Map.Make(GroupID)

type context = {
  basedir : string;
  mutable logmap : out_channel LogMap.t
}

let open_log ctx (room, server) =
  let tm = localtime (gettimeofday ()) in
  let year = tm.tm_year + 1900 in
  let month = tm.tm_mon + 1 in
  let day = tm.tm_mday in
  let p1 = Filename.concat ctx.basedir (room ^ "@" ^ server) in
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
             room server day month year room server day month year);
        flush out_log;
        out_log
    else
      open_out_gen [Open_append] 0o644 file

let close_log ctx room =
  let lf = LogMap.find room ctx.logmap in
    output_string lf "</body>\n</html>";
    flush lf;
    close_out lf;
    ctx.logmap <- LogMap.remove room ctx.logmap
        
let rotate_logs ctx () =
  log#info "MUC Log: Rotating chatlogs";
  ctx.logmap <-
    LogMap.mapi (fun room lf ->
                   let old = lf in
                   let newlog = open_log ctx room in
                     output_string old "</body>\n</html>";
                     flush old;
                     close_out old;
                     newlog) ctx.logmap
    
let add_chatlog ctx jid =
  let room = jid.lnode, jid.ldomain in
  let out_log = open_log ctx room in
    ctx.logmap <- LogMap.add room out_log ctx.logmap;
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

let write ctx jid_room text =
  let out_log =
    try
      LogMap.find (jid_room.lnode, jid_room.ldomain) ctx.logmap
    with Not_found ->
      add_chatlog ctx jid_room
  in
  let curtime = 
    Strftime.strftime ~tm:(localtime (gettimeofday ())) "%H:%M" in
    output_string out_log 
      (Printf.sprintf 
         "[%s] %s<br>\n"
         curtime text);
    flush out_log

let muc_log_message ctx from env stanza =
  match stanza.content.subject with
    | None -> (
        match stanza.content.body with
          | None -> ()
          | Some body ->
              if from.lresource <> "" then
                if body <> "" then
                  write ctx from (
                    if body = "/me" then
                      Printf.sprintf "* %s" from.resource
                    else if String.length body > 3 && 
                      String.sub body 0 4 = "/me " then
                        Printf.sprintf "* %s %s" from.resource
                          (html_url (Common.string_after body 4))
                    else
                      make_message from.resource body)
                else
                  ()
      )
    | Some subject ->
        if from.lresource <> "" then
          write ctx from
            (Lang.get_msg env.env_lang "muc_log_set_subject" 
               [from.resource;  html_url subject])
        else
          write ctx from
            (Lang.get_msg env.env_lang "muc_log_subject" [html_url subject])

let muc_log_event ctx muc_context xmpp env jid_from = function
  | MUC_join ->
      write ctx jid_from
        ("-- " ^ (Lang.get_msg env.env_lang "muc_log_join" 
                    [jid_from.resource]))
  | MUC_leave reason ->
      write ctx jid_from
        ("-- " ^
           match reason with
             | None ->
                 Lang.get_msg env.env_lang "muc_log_leave"
                   [jid_from.resource]
             | Some v ->
                 Lang.get_msg env.env_lang "muc_log_leave_reason"
                   [jid_from.resource; html_url v]
        )
  | MUC_kick reason ->
      write ctx jid_from
        ("-- " ^
           match reason with
             | None ->
                 Lang.get_msg env.env_lang "muc_log_kick"
                   [jid_from.resource]
             | Some v ->
                 Lang.get_msg env.env_lang "muc_log_kick_reason"
                   [jid_from.resource; html_url v]
        )
  | MUC_ban reason ->
      write ctx jid_from
        ("-- " ^
           match reason with
             | None ->
                 Lang.get_msg env.env_lang "muc_log_ban"
                   [jid_from.resource]
              | Some v ->
                  Lang.get_msg env.env_lang "muc_log_ban_reason"
                    [jid_from.resource; html_url v]
        )
  | MUC_members_only reason ->
      write ctx jid_from
        ("-- " ^
           match reason with
             | None ->
                 Lang.get_msg env.env_lang "muc_log_unmember"
                   [jid_from.resource]
             | Some v ->
                 Lang.get_msg env.env_lang "muc_log_unmember_reason"
                   [jid_from.resource; html_url v]
        )
  | MUC_nick (newnick, reason) ->
      write ctx jid_from
        ("-- " ^
           (Lang.get_msg env.env_lang 
              "muc_log_change_nick" [jid_from.resource; newnick]))
  | _ ->
      ()
        
let process_message ctx muc_context xmpp env stanza hooks =
  match stanza.jid_from with
    | None -> do_hook xmpp env stanza hooks
    | Some from ->
        if is_joined muc_context from && stanza.kind = Some Groupchat then
          muc_log_message ctx from env stanza;
        do_hook xmpp env stanza hooks

let plugin opts =
  let basedir = get_value opts "dir" "chatlogs" "chatlogs" in
    Muc.add_for_muc_context
      (fun muc_context xmpp ->
         let ctx = {
           basedir = basedir;
           logmap = LogMap.empty
         } in
         let _ = Scheduler.add_task timerQ (rotate_logs ctx)
           (get_next_time 0 0 ()) (get_next_time 0 0); in
           Muc.add_muc_event_handler muc_context (muc_log_event ctx);
           Hooks.add_message_hook xmpp 11 "muc_log"
             (process_message ctx muc_context)
      )

let () =
  Plugin.add_plugin "muc_log" plugin
