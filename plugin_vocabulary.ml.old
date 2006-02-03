(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xml
open Common
open Dbm

let open_dbm () =
   let name = get_attr_s Config.config ~path:["plugins"; "vocabulary"] "name" in
   Dbm.opendbm "vocabulary" [Dbm_create; Dbm_rdwr] 0o644 

let dfn text from event xml out =
   try
      let eq = String.index text '=' in
      let w1 = trim (String.sub text 0 eq) in
      let w2 = trim (string_after text (eq+1)) in

      let db = open_dbm () in
      let () = 
	 try
	    Dbm.add db w1 w2 
	 with Dbm_error _ ->
	    Dbm.replace db w1 w2
      in
	 close db;
	 out (make_msg xml 
		 (Lang.get_msg ~xml "plugin_vocabulary_wrote" []))
   with Not_found ->
      out (make_msg xml 
	      (Lang.get_msg ~xml "plugin_vocabulary_invalid_syntax" []))

let wtf text event from xml out =
   if text = "" then
      out (make_msg xml 
	      (Lang.get_msg ~xml "plugin_vocabulary_invalid_syntax" []))
   else
      let query =
	 try
	    let q = String.index text '?' in
	       String.sub text 0 q
	 with Not_found -> text in
      let db = open_dbm () in
      let () = 
	 try
	    let reply = Dbm.find db query in
	       out (make_msg xml reply)
	 with Not_found ->
	    out (make_msg xml 
		    (Lang.get_msg ~xml "plugin_vocabulary_not_found" []))
      in
	 close db

let _ =
   Hooks.register_handle (Hooks.Command ("wtf", wtf));
   Hooks.register_handle (Hooks.Command ("dfn", dfn));
