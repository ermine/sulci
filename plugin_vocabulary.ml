open Xml
open Common
open Dbm

let open_dbm () =
   let name = get_attr_s Config.config ~path:["plugins"; "vocabulary"] "name" in
   Dbm.opendbm "vocabulary" [Dbm_create; Dbm_rdwr] 0o644 

let dfn text xml out =
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
	 out (make_msg xml "Записал.")
   with Not_found ->
      out (make_msg xml "Ась?")

let wtf text xml out =
   if text = "" then
      out (make_msg xml "ну?")
   else
      let db = open_dbm () in
      let () = 
	 try
	    let reply = Dbm.find db text in
	       out (make_msg xml reply)
	 with Not_found ->
	    out (make_msg xml "хз")
      in
	 close db

let _ =
   Hooks.register_handle (Hooks.Command ("wtf", wtf));
   Hooks.register_handle (Hooks.Command ("dfn", dfn));
