(*                                                                          *)
(* (c) 2006 Anastasia Gornostaeva. <ermine@ermine.pp.ru>                    *)
(*                                                                          *)

open Unix
open Common
open Types
open Http_suck

let rex = Pcre.regexp "^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$"

let hostip text event from xml out =
   let proc () =
      let ip =
	 try
	    let h = gethostbyname text in
	       Some (string_of_inet_addr h.h_addr_list.(0))
	 with Not_found ->
	    if Pcre.pmatch ~rex text then
	       Some text
	    else
	       None
      in
	 match ip with
	    | None ->
		 make_msg out xml
		    (Lang.get_msg ~xml "plugin_hostip_bad_syntax" []);
	    | Some ip ->
		 let url = Printf.sprintf 
		    "http://api.hostip.info/get_html.php?ip=%s&position=true" 
		    ip in
		 let callback data =
		    match data with
		       | OK content ->
			    make_msg out xml ("\n" ^ (Xml.encode content))
		       | _ ->
			    make_msg out xml (Lang.get_msg ~xml 
						 "plugin_hostip_failed" [])
		 in
		    Http_suck.http_get url callback
   in
      ignore (Thread.create proc ())

let _ =
   Hooks.register_handle (Hooks.Command ("hostip", hostip));
