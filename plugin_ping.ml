open Xml
open Xmpp
open Types

let groupchat_rex = Str.regexp "ping\\( +\\(.+\\)\\)?"
let chat_rex = Str.regexp "ping"

let ping xml out bot mynick lang =
   let body = try Xml.get_cdata xml ~path:["body"] with _ -> "" in
   let from = Xml.get_attr_s xml "from" in
   let now = Unix.gettimeofday () in
   let muc_type = safe_get_attr_s xml "type" in
      if muc_type = "groupchat" then begin	 
	 if Str.string_match groupchat_rex body 0 then
	    let to_ = (try 
			  let n = Str.matched_group 2 body in
			     get_bare_jid from ^ "/" ^ n
		       with _ -> from) in
	    let proc x =
	       let asker = get_resource from in
	       let nick = get_resource (to_) in
	       let reply = match safe_get_attr_s x "type" with
		  | "result" ->
		       let diff = Unix.gettimeofday () -. now in
			  if nick = mynick then
			     Printf.sprintf 
		    "%s: скорость отклика сервера для меня составляет %f секунд"
				asker diff
			  else
			     Printf.sprintf "%s: pong reply from %s: %f"
				asker
				(if asker = nick then "you" else nick)
				diff
		  | "error" ->
		       let err_text =  
			  try 
			     get_cdata ~path:["error"; "text"] x 
			  with _ -> "not found" 
		       in
			  asker ^ ": " ^ err_text ^ " [" ^ nick ^ "]"
	       in
		  out (Xmlelement ("message", ["to", get_bare_jid to_; 
					       "type", muc_type],
				   [make_simple_cdata "body" reply]))	
	    in
	    let id = new_id () in
	       Event.sync (Event.send bot (RegisterHandle (Id (id, proc))));
	       out (Iq.iq_query "jabber:iq:version" to_ id)
      end
      else if muc_type = "chat" then
	 if Str.string_match chat_rex body 0 then
	    let proc x =
	       let reply = match get_attr_s x "type" with
		  | "result" ->
		       let diff = Unix.gettimeofday () -. now in
			  "pong reply from you: " ^ string_of_float diff
		  | "error" ->
		       let err_text =  
			  try 
			     get_cdata ~path:["error"; "text"] x 
			  with _ -> "not found" 
		       in
			  err_text
	       in
		  out (Xmlelement ("message", ["to", from; "type", muc_type],
				   [make_simple_cdata "body" reply]))
	    in
	    let id = new_id () in
	       Event.sync (Event.send bot (RegisterHandle (Id (id, proc))));
	       out (Iq.iq_query "jabber:iq:version" from id)
  
let _ =
   Muc.register_cmd "ping" ping;
   Muc.register_help "ping"
"ping
   Пинговать себя
ping nick
   Пинговать юзера"
