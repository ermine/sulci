(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Unix
open Common

let dns text xml out =
   if text = "" then
      out (make_msg xml (Lang.get_msg ~xml "plugin_misc_dns_invalid_syntax" []))
   else
      out (make_msg xml
	      (try
		  let inet_addr = inet_addr_of_string text in
		  let s = gethostbyaddr inet_addr in
		     s.h_name ^ " " ^
			(String.concat " "
			    (Array.to_list s.h_aliases))
	       with Failure _ ->
		  try 
		     let h = gethostbyname text in
		     let hl = Array.to_list h.h_addr_list in
			String.concat " "
			   (List.map (fun addr -> string_of_inet_addr addr) hl)
		  with Not_found ->  
		     Lang.get_msg ~xml "plugin_misc_dns_not_resolved" []))

let _ =
   Hooks.register_handle (Hooks.Command ("dns", dns))
