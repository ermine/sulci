open Xml
open Xmlstring
open Xmpp
open Types

type m = MFrom of string | MId of string | MXmlns of string
     
module XId =
struct
   type t = m
   let compare = compare
end
module Xmap = Map.Make(XId)

let process_xml xmap xml =
   (try
       let f = Xmap.find (MFrom (get_bare_jid (get_attr_s xml "from"))) xmap in
          f xml
    with _ -> ());
   let xmp = 
      if get_tagname xml = "iq" then
	 if (safe_get_attr_s xml "type" = "result") ||
	     (safe_get_attr_s xml "type" = "error") &&
	    safe_get_attr_s xml "id" <> "" then
	    try
               let f = Xmap.find (MId (get_attr_s xml "id")) xmap in
		  f xml;
		  Xmap.remove (MId (get_attr_s xml "id")) xmap
	    with _ -> xmap
	 else begin
	    if safe_get_attr_s xml "type" = "get" then
               (try
		   let f = Xmap.find (MXmlns (get_xmlns xml)) xmap in
                      f xml;
		with _ -> ());
	    xmap;
	 end
      else
	 xmap
   in
      xmp

let _ = 
   let server = trim (Xml.get_cdata Config.conf ~path:["jabber"; "server"]) in
   let port = int_of_string (trim (Xml.get_cdata Config.conf 
				      ~path:["jabber"; "port"])) in
   let user = trim (Xml.get_cdata Config.conf ~path:["jabber"; "user"]) in
   let password = trim (Xml.get_cdata Config.conf 
			   ~path:["jabber"; "password"]) in
   let resource = trim (Xml.get_cdata Config.conf 
			   ~path:["jabber"; "resource"]) in
   let debuglog = trim (Xml.get_cdata Config.conf 
			   ~path:["debug"; "logfile"]) in

   let jid, out, next_xml = 
      Xmpp.client user password resource ~logfile:debuglog server in

   let queue = Event.new_channel () in

   let rec new_xml () =
      Event.sync (Event.send queue (Xml (next_xml ())));
      new_xml ()
   in
   let rec session_loop xmap =
      match Event.sync (Event.receive queue) with
         | Xml xml ->
              session_loop (process_xml xmap xml);
         | RegisterHandle rdata ->
              match rdata with
                 | From (jid, proc) ->
                      session_loop (Xmap.add (MFrom jid) proc xmap)
                 | Xmlns (name, proc) ->
                      session_loop (Xmap.add (MXmlns name) proc xmap)
                 | Id (id, proc) ->
                      session_loop (Xmap.add (MId id) proc xmap)

   in
   let xmap = List.fold_left
                 (fun xmap (k, f) -> Xmap.add k f xmap) Xmap.empty
                 [MXmlns "jabber:iq:version", Iq.iq_version_reply out;
                  (* "jabber:iq:last", Iq.iq_last_reply *)
                 ]
   in
      ignore (Thread.create new_xml ());
      let t = Thread.create session_loop xmap in
         Muc.start queue out;
         Thread.join t
