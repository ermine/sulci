(*                                                                          *)
(* (c) 2004, Anastasia Gornostaeva. <ermine@ermine.pp.ru                    *)
(*                                                                          *)

open Xml

type r =
   | From of string * (element -> unit)
   | Xmlns of string * (element -> unit)
   | Id of string * (element -> unit)

type t = 
   | Xml of element 
   | RegisterHandle of r

let trim str =
   let r = Str.regexp "[^ \t\n\r]+" in
   let p1 = Str.search_forward r str 0 in
   let p2 = Str.search_backward r str (String.length str) in
      String.sub str p1 (p2-p1+1)

let my_id = ref 0

let new_id () = 
   incr my_id;
   "stoat_" ^ string_of_int !my_id
   
let outmsg from body =
   Xmlelement ("message", ["to", from;
			   "type", "groupchat"],
	       [make_simple_cdata "body" body])

let make_msg xml response =
   let from = Xml.get_attr_s xml "from" in
   let nick = Xmpp.get_resource from in
      match safe_get_attr_s xml "type" with
	 | "groupchat" ->
	      Xmlelement ("message", ["to", Xmpp.get_bare_jid from;
				      "type", "groupchat"],
			  [make_simple_cdata "body" (nick ^ ": " ^ response)])
	 | other ->
	      Xmlelement ("message", 
		       (match other with
			  | "" -> ["to", from]
			  | o -> ["to", from; "type", other]),
		       [make_simple_cdata "body" response])
