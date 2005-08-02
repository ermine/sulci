(*                                                                          *)
(* (c) 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>                    *)
(*                                                                          *)

open Xmpp
open Xml
open Types
open Common

let _ =
   let result_vcard alist =
      if alist = [] then
	 "Нет сведений"
      else
	 let r1 = try List.assoc "FN" alist with Not_found -> "" in
	 let r2 = try 
	    (if r1 = "" then "" else r1 ^ " -- ") ^
	       List.assoc "DESC" alist with Not_found -> r1 in 
	 let r3 = try
	    (if r2 = "" then "" else r2 ^ " -- ") ^
	       List.assoc "URL" alist with Not_found -> r2 in
	 let r4 = try
	    (if r3 = "" then "" else r3 ^ " -- ") ^ "Email: " ^
	       List.assoc "EMAIL" alist with Not_found -> r3 in
	    r4
   in
   let vsearch =
      let parse_vcard origxml event from xml out =
	 match event with
	    | Iq (_, type_, "")
	    | Iq (_, type_, "vcard-temp") ->
		 (match type_ with
		     | `Result ->
			  let rec aux_scan tail acc =
			     match tail with
				| [] -> acc
				| h :: t ->
				     match h with
					| Xmlelement _ ->
					     let name = get_tagname h in
					     let value = get_cdata h in
						if trim(value) <> "" then
						   aux_scan t 
						      ((name, value)::acc)
						else
						   aux_scan t acc
					| _ -> aux_scan t acc
			  in
			  let res = aux_scan 
			     (try get_subels xml ~path:["vCard"] 
			      with Not_found -> []) [] in
			     
			     out (make_msg origxml (result_vcard res))
		     | `Error ->
			  out (make_msg origxml "error getting vcard")
		     | _ -> ())
	    | _ -> ()
      in
	 fun text event from xml out ->
	    match event with
	       | MUC_message _ ->
		    let room = from.luser, from.lserver in
		    let room_env = GroupchatMap.find room !groupchats in
		       if Nicks.mem text room_env.nicks then
			  let id = new_id () in
			     Hooks.register_handle (Hooks.Id 
						       (id, parse_vcard xml));
			     out (Jeps.iq_vcard_query ~id 
				     (from.user ^ "@" ^ from.server ^ "/"
					 ^ text))
		       else 
			  try
			     let to_ =
				let jid = jid_of_string text in
				   if jid.luser = "" && jid.lresource = "" then
				      begin
					 dnsprep jid.lserver;
					 jid.server
				      end
				   else
				      jid.user ^ "@" ^ jid.server
			     in
			     let id = new_id () in
				Hooks.register_handle 
				   (Hooks.Id (id, parse_vcard xml));
				out (Jeps.iq_vcard_query ~id to_)
			  with _ ->
			     out (make_msg xml 
				     "/me озирается в поисках дубинки")
   in
      Hooks.register_handle (Hooks.Command ("vcard", vsearch))
