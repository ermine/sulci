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
	    else str
      in
	 cycle (pred (String.length str))

let trim str =
   let r1 = skip_ws str in
      rskip_ws r1

let make_msg xml response =
   let from = Xml.get_attr_s xml "from" in
   let nick = Xmpp.get_resource from in
      match safe_get_attr_s xml "type" with
	 | "groupchat" ->
	      Xmlelement ("message", ["to", Xmpp.get_bare_jid from;
				      "type", "groupchat"],
			  [make_simple_cdata "body" 
			      (if Pcre.pmatch ~pat:"/me" response then
				  response
			       else
				  if nick = "" then response
				  else (nick ^ ": " ^ response)
			      )])
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

let print_exn exn =
   Printf.eprintf "Catched exception in hooks.ml: %s\n"
      (Printexc.to_string exn)

