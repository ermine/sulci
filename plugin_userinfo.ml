open Xml
open Xmpp
open Types
open Unix

let rex = Str.regexp "\\([a-z]+\\) +\\(.+\\)"

let reply_version xml asker nick mynick =
   let client = get_cdata xml ~path:["query"; "name"] in
   let version = get_cdata xml ~path:["query"; "version"] in
   let os = get_cdata xml ~path:["query"; "os"] in
      if nick = mynick then
	 Printf.sprintf "%s: %s %s - %s" asker client version os
      else
	 Printf.sprintf "%s: у %s клиент %s %s - %s"
	    asker
	    (if asker = nick then "тебя" else nick)
	    client version os

let reply_idle xml asker nick mynick =
   let seconds = get_attr_s xml ~path:["query"] "seconds" in
      if nick = mynick then
	 "я вообще не молчу!"
      else
	 let idle = 
	    let hours, mins, secs = 
	       Strftime.seconds_to_string (int_of_string seconds) in
	       (if hours = 0 then "" else (string_of_int hours) ^ " ч. ") ^
	       (if mins = 0 then "" else (string_of_int mins) ^ " мин. ") ^
	       (string_of_int secs) ^ " сек." in
	    Printf.sprintf "%s: %s молчит %s"
	       asker nick idle
	    
let reply_time xml asker nick mynick =
   let utc = get_cdata xml ~path:["query"; "utc"] in
   let tz = get_cdata xml ~path:["query"; "tz"] in
   let display = get_cdata xml ~path:["query"; "display"] in
      if mynick = nick then
	 Printf.sprintf "у меня в компьютере часы показывают %s" display
      else 
	 Printf.sprintf "%s: На часах у %s показывается %s" 
	    asker (if asker = nick then "тебя" else nick) display
				   
let req = ["version", "jabber:iq:version", reply_version;
	   "idle", "jabber:iq:last", reply_idle;
	   "time", "jabber:iq:time", reply_time]

let userinfo xml out bot mynick lang =
   let body = try Xml.get_cdata xml ~path:["body"] with _ -> "" in
   let from = Xml.get_attr_s xml "from" in
      if safe_get_attr_s xml "type" = "groupchat" && 
	 Str.string_match rex body 0 then
	 let request = Str.matched_group 1 body in
	    try (
	       let (_, xmlns, reply_proc) =
		  List.find (function (key, xmlns, proc) -> key = request) req 
	       in
	       let to_ = (try 
			     let n = Str.matched_group 2 body in
				get_bare_jid from ^ "/" ^ n
			  with _ -> from) in
	       let proc x =
		  let asker = get_resource from in
		  let nick = get_resource (to_) in
		  let reply = match get_attr_s x "type" with
		     | "result" ->
			  reply_proc x asker nick mynick
		     | "error" ->
			  let err_text =  
			     try 
				get_cdata ~path:["error"; "text"] x 
			     with _ -> "not found" 
			  in
			     asker ^ ": " ^ err_text ^ " [" ^ nick ^ "]"
		  in
		     out (Xmlelement ("message", ["to", get_bare_jid to_; 
						  "type", "groupchat"],
				      [make_simple_cdata "body" reply]))	
	       in
	       let id = new_id () in
		  Event.sync (Event.send bot (RegisterHandle (Id (id, proc))));
		  out (Iq.iq_query xmlns to_ id)
	    ) with _ -> ()
   
(*
let greeting xml out bot mynick lang =
   let body = try Xml.get_cdata xml ~path:["body"] with _ -> "" in
   let from = Xml.get_attr_s xml "from" in
      if safe_get_attr_s xml "type" = "groupchat" && 
*)  

let _ =
   Muc.register_cmd "version" userinfo;
   Muc.register_help "version"
"version nick
   Вывод информации о клиенте юзера";
   Muc.register_cmd "idle" userinfo;
   Muc.register_help "idle"
"idle nick";
   Muc.register_cmd "time" userinfo;
   Muc.register_help "time"
"time nick
   Вывод информации о текущих показаниях часов на компьютере юзера"
