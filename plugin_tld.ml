(*                                                                          *)
(* (c) 2004, 2005, 2006 Anastasia Gornostaeva. <ermine@ermine.pp.ru>        *)
(*                                                                          *)

open Common
open Dbm

let tlds = opendbm "tlds" [Dbm_rdonly] 0o666

let tld text event from xml out =
   if text = "" then
      make_msg out xml "какой домен?"
   else
      if text = "*" then
	 make_msg out xml "http://www.iana.org/cctld/cctld-whois.htm"
      else
	 let tld = if text.[0] = '.' then string_after text 1 else text in
	    make_msg out xml (try Dbm.find tlds tld with Not_found -> 
				 "неизвестный домен")

let _ =
   Hooks.register_handle (Hooks.Command ("tld", tld))
