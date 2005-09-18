(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xml

let string_after s n =
  String.sub s n (String.length s - n)

let skip_ws str =
   if str = "" then str
   else
      let rec cycle i =
	 if i = String.length str then ""
	 else
	    if List.mem str.[i] [' '; '\n'; '\r'; '\t'] then cycle (succ i)
	    else if i > 0 then string_after str i
	    else str
      in
	 cycle 0

let rskip_ws str =
   if str = "" then str
   else
      let rec cycle i =
	 if i = -1 then ""
	 else
	    if List.mem str.[i] [' '; '\n'; '\t'; '\r'] then cycle (pred i)
	    else String.sub str 0 (i+1)
      in
	 cycle (pred (String.length str))

let trim str =
   let r1 = skip_ws str in
      rskip_ws r1

let msg_limit = ref 
   (try int_of_string (get_attr_s Config.config ~path:["muc"] "limit")
    with Not_found -> 450)

let make_msg xml response =
   let from = Xml.get_attr_s xml "from" in
   let nick = Xmpp.get_resource from in
      match safe_get_attr_s xml "type" with
	 | "groupchat" ->
	      if String.length response < !msg_limit then
		 Xmlelement ("message", ["to", Xmpp.get_bare_jid from;
					 "type", "groupchat"],
			     [make_simple_cdata "body" 
				 (if Pcre.pmatch ~pat:"/me" response then
				     response
				  else
				     if nick = "" then response
				     else (nick ^ ": " ^ response)
				 )])
	      else begin
		 Xmlelement ("message", ["to", from; "type", "chat"],
			     [make_simple_cdata "body" response])
	      end
	 | other ->
	      Xmlelement ("message", 
		       (match other with
			  | "" -> ["to", from]
			  | o -> ["to", from; "type", other]),
			  [make_simple_cdata "body" response])


let get_error_semantic xml =
   let err_text =  
      try 
	 get_cdata ~path:["error"; "text"] xml
      with _ -> raise Not_found
   in err_text

(* temp code *)
exception DNSPrepError

let dnsprep str =
   if String.contains str '.' then ()
   else raise DNSPrepError
