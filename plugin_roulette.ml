open Xml
open Xmpp
open Types

let rex = Str.regexp "tryme"
let rex1 = Str.regexp "try \\(.+\\)$"

let r = Random.self_init ()

let roulette xml out bot mynick lang =
   let body = try Xml.get_cdata xml ~path:["body"] with _ -> "" in
(*
   let from = get_attr_s xml "from" in
   let nick = 
      if Str.string_match rex body 0 then
	 get_resource from
      else if Str.string_match rex1 body 0 then
	 Str.matched_group 1 body
      else
	 ""
   in
      if nick <> "" then
*)
      if Str.string_match rex body 0 then
	 if Random.int 10 = 1 then
	    let from = get_attr_s xml "from" in
	    let nick = get_resource from in
	    let conf = get_bare_jid from in
	    let id = new_id () in
	       out (Muc.kick id conf nick "Пшииик!");
	       let proc x = 
		  let reply = match get_attr_s x "type" with
		     | "result" ->
			  "Адью."
		     | "error" ->
			  let err_text =  
			     try 
				get_cdata ~path:["error"; "text"] x 
			     with _ -> "Ыыыы..." 
			  in
			     nick ^ ": " ^ err_text
		  in
		     out (Xmlelement ("message", ["to", conf; 
						  "type", "groupchat"],
				      [make_simple_cdata "body" reply]))	
	       in
		  Event.sync (Event.send bot (RegisterHandle (Id (id, proc))))
	 else
	    out (make_msg xml "Как-нить в следующий раз...")

let _ =
   Muc.register_cmd "tryme" roulette;
