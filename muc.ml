(*                                                                          *)
(* (c) 2004, Anastasia Gornostaeva. <ermine@ermine.pp.ru                    *)
(*                                                                          *)

open Xml
open Xmpp
open Types

module Id =
struct
   type t = string
   let compare = compare
end

module PMap = Map.Make(Id)  (* мап мапов для конфы *)
module Nicks = Map.Make(Id) (* мап ников для одной конфы *)
let participants = ref PMap.empty

module Help = Map.Make(Id)  (* мап хелпов *)
let hmap = ref Help.empty

module CmdMap = Map.Make(Id) (* плагины *)
let cmdmap = ref CmdMap.empty

let catchlist = ref [] 

(*
module Hooks = Map.Make(Id)
let phooks = ref PMap.empty
let mhooks = ref ...
*)

type muc_cmd = | MUC_Join of string | MUC_Leave of string 
	       | MUC_ChangeNick of string * string * string
	       | MUC_Ignore

let process_presence mynick bot xml out lang =
   let from = get_attr_s xml "from" in
   let user = get_resource from in
   let room = get_bare_jid from in
   let nmap = PMap.find room !participants in
   let cmd = match safe_get_attr_s xml "type"  with
      | "" -> 
	   if user <> mynick && not (Nicks.mem user nmap) then
	      let newnmap = Nicks.add user user nmap in
		 participants := PMap.add room newnmap !participants;
		 MUC_Join user
	   else
	      MUC_Ignore
      | "unavailable" -> 
	   let x = 
	      List.find (function 
			    | Xmlelement ("x", attrs, _) ->
				 if
				    (try List.assoc "xmlns" attrs with _ -> "")=
				    "http://jabber.org/protocol/muc#user" 
				 then true else false
			    | _ -> false
			) (Xml.get_subels xml) in
	      (match safe_get_attr_s xml ~path:["status"] "code" with
		  | "303" -> (* /nick *)
		      let newnick = get_attr_s xml ~path:["x"; "item"] "nick" in
		      let orignick = Nicks.find user nmap in
			 participants := PMap.add room
			   (Nicks.add newnick orignick (Nicks.remove user nmap))
					    !participants;
			 MUC_ChangeNick (newnick, user, orignick)
		 | "307" (* /kick *)
		 | "301" (* /ban *)
		 | "321" (* non-member *)
		 | _ -> 
		      participants := PMap.add room (Nicks.remove user nmap) 
			  !participants;
		      MUC_Leave user
	      )
      | _ -> MUC_Ignore
   in ()

let cmd_rex = Str.regexp "\\([^: \r\n\t]+\\)\\([ \n\r\t]\\|$\\)"

let process_message mynick bot xml out lang =
   let from = get_attr_s xml "from" in
   let user = get_resource from in

      if user <> mynick then
	 let body = try Xml.get_cdata xml ~path:["body"] with _ -> "" in
	 if Str.string_match cmd_rex body 0 then
	    let word = Str.matched_group 1 body in
	       try
		  let proc = CmdMap.find word !cmdmap in
		     proc xml out bot mynick lang
	       with Not_found ->	
		  List.iter (function proc ->
				proc xml out bot mynick lang
			    ) !catchlist
	 else
	    List.iter (function proc ->
			  proc xml out bot mynick lang
		      ) !catchlist
      else
	 ()

let dispatch nick xml out bot lang =
   if get_tagname xml = "presence" then
      process_presence nick bot xml out lang
   else 
      if get_tagname xml = "message" then
	 if not (mem_xml xml ["message"] "x" ["xmlns", "jabber:x:delay"]) &&
	    not (mem_xml xml ["message"] "subject" []) then
	    process_message nick bot xml out lang

let join_room nick room =
   make_presence 
      ~subels:
      [Xmlelement ("x", ["xmlns", "http://jabber.org/protocol/muc"], [])]
      (room ^ "/" ^ nick)

let register_cmd cmd proc =
   cmdmap := CmdMap.add cmd proc !cmdmap
let register_help key descr =
   hmap := Help.add key decr !hmap
let register_catch proc =
   catchlist := proc :: !catchlist
(*
let unregister_catc key proc =
   catchlist := List.remove_assoc key !catchlist
*)
   
let start bot out =
   let user = trim (Xml.get_cdata Config.conf ~path:["jabber"; "user"]) in
   let rooms = Xml.get_subels ~path:["muc"] ~tag:"room" Config.conf in
   let muc_ev = Event.new_channel () in

   let rec session_loop () =
      let (nick, lang, xml) = Event.sync (Event.receive muc_ev) in
	 dispatch nick xml out bot lang;
	 session_loop ()
   in

   let proc nick lang xml =
      Event.sync (Event.send muc_ev (nick, lang, xml))
   in
      ignore (Thread.create session_loop ());
      List.iter (fun r ->
		    let mynick = try Xml.get_attr_s r "nick" with _ -> user
		    and roomname = Xml.get_attr_s r "jid"
		    and lang = try Xml.get_attr_s r "lang" with _ -> "ru" in

		       participants := PMap.add roomname Nicks.empty 
			  !participants;
		       Event.sync (Event.send bot
				      (RegisterHandle 
					  (From (roomname, proc mynick lang))));
		       out (join_room mynick roomname);
		) rooms

(* **** *)
let kick id room nick reason =
   Xmlelement ("iq", ["to", room; "type", "set"; "id", id],
	       [Xmlelement ("query", ["xmlns",
				      "http://jabber.org/protocol/muc#admin"],
			    [Xmlelement ("item", ["nick", nick; "role", "none"],
					 [make_simple_cdata "reason" reason]
					)])])
