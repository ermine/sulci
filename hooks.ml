(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xml
open Xmpp
open Common
open Types

module IdMap = Map.Make(Id)
let idmap = ref IdMap.empty

module XmlnsMap = Map.Make(Id)
let xmlnsmap = ref XmlnsMap.empty

let presencemap = ref []

module CommandMap = Map.Make(Id)
let commands = ref CommandMap.empty

let onstart =
   let on_start out =
      GroupchatMap.iter (fun room env ->
			    out (Muc.join_room env.mynick 
				    (string_of_jid env.room))) !groupchats
   in
      ref [on_start]

let onquit = ref []
let catchset = ref []
let filters = ref []

type reg_handle =
   | Xmlns of 
	string * (xmpp_event -> jid -> element -> (element -> unit) -> unit)
   | Id of string * (xmpp_event -> jid -> element -> (element -> unit) -> unit)
   | Command of string * 
	(string -> xmpp_event -> jid -> element -> (element -> unit) -> unit)
   | OnStart of ((element -> unit) -> unit)
   | OnQuit of ((element -> unit) -> unit)
   | Catch of (xmpp_event -> jid -> element -> (element -> unit) -> unit)
   | Filter of (xmpp_event -> jid -> element -> (element -> unit) -> unit)
   | PresenceHandle of 
	(xmpp_event -> jid -> element -> (element -> unit) -> unit)

let register_handle (handler:reg_handle) =
   match handler with
      | Xmlns (xmlns, proc) ->
	   xmlnsmap := XmlnsMap.add xmlns proc !xmlnsmap
	   (* hookmap := HookMap.add (HookXmlns xmlns) proc !hookmap *)
      | Id (id, proc) ->
	   idmap := IdMap.add id proc !idmap
	   (* hookmap := HookMap.add (HookId id) proc !hookmap *)
      | Command (command, proc) ->
	   commands := CommandMap.add command proc !commands
      | OnStart proc ->
	   onstart := proc :: !onstart;
      | OnQuit proc ->
	   onquit := proc :: !onquit;
      | Catch proc ->
	   catchset := proc :: !catchset
      | Filter proc ->
	   filters := proc :: !filters
      | PresenceHandle proc ->
	   presencemap := proc :: !presencemap
	   (*
	   hookmap := HookMap.add HookPresence 
	      (try proc :: (HookMap.find HookPresence !hookmap)
	       with Not_found -> [proc]) !hookmap
	   *)

let process_iq event from xml out =
   let id = safe_get_attr_s xml "id" in
      if id <> "" then
	 match event with
	    | Iq `Result
	    | Iq `Error ->
		 begin try
		    let f = IdMap.find id !idmap in
		       (try f event from xml out with exn -> print_exn exn ~xml);
		       idmap := IdMap.remove id !idmap
		 with Not_found -> ()
		 end
	    | Iq `Get
	    | Iq `Set ->
		 begin try
		    let f = XmlnsMap.find (get_xmlns xml) !xmlnsmap in
		       (try f event from xml out with exn -> print_exn exn ~xml);
		 with Not_found -> ()
		 end
	    | _ -> ()
      else 
	 match event with
	    | Iq `Get
	    | Iq `Set ->
		 begin try
		    let f = XmlnsMap.find (get_xmlns xml) !xmlnsmap in
		       (try f event from xml out with exn -> print_exn exn ~xml);
		 with Not_found -> ()
		 end
	    | _ -> ()

let do_command text event from xml out =
   let word = 
      try String.sub text 0 (String.index text ' ') with Not_found -> text in
   let f = CommandMap.find word !commands in
   let params = try string_after text (String.index text ' ') with _ -> "" in
      try f (trim params) event from xml out with exn -> print_exn exn ~xml

let process_message event from xml out =
   match event with
      | MUC_message (msg_type, nick, text) ->
	   if msg_type <> `Error then
	      if text <> "" then
		 try
		    let room_env = GroupchatMap.find (from.luser, from.lserver)
		       !groupchats in
		       begin match msg_type with
			  | `Groupchat ->
			       if from.lresource <> room_env.mynick && 
				  nick = "" then
				     do_command text event from xml out
			       else
				  raise Not_found
			  | _ ->
			       do_command text event from xml out
		       end
		 with Not_found ->
		    List.iter  (fun f -> 
				   try f event from xml out with exn ->
				      print_exn exn ~xml
			       ) !catchset 
	      else
		 List.iter  (fun f -> 
				try f event from xml out with exn ->
				   print_exn exn ~xml
			    ) !catchset 
      | Message ->
	   if safe_get_attr_s xml "type" <> "error" then
	      let text = 
		 try get_cdata xml ~path:["body"] with Not_found -> "" in
		 try do_command text event from xml out with Not_found ->
		    List.iter  (fun f -> 
				   try f event from xml out with exn ->
				      print_exn exn ~xml
			       ) !catchset

exception FilteredOut
  
let rec process_xml next_xml out =
   let xml = next_xml () in
   let from = safe_jid_of_string (get_attr_s xml "from") in
   let room = (from.luser, from.lserver) in
   let tag = get_tagname xml in
   let event =
      if GroupchatMap.mem room !groupchats then
	 match tag with
	    | "presence" ->
		 Muc.process_presence from xml out
	    | "message" ->
		 Muc.process_message from xml out
	    | "iq" ->
		 let iq_type = match safe_get_attr_s xml "type" with
		    | "get" -> `Get
		    | "set" -> `Set
		    | "result" -> `Result
		    | "error" -> `Error
		    | _ -> `InvalidType
		 in
		    Iq iq_type
	    | _ -> InvalidStanza xml
      else
	 match tag with
	    | "message" -> Message
	    | "presence" -> Presence
	    | "iq" ->
		 let iq_type = match safe_get_attr_s xml "type" with
		    | "get" -> `Get
		    | "set" -> `Set
		    | "result" -> `Result
		    | "error" -> `Error
		    | _ -> `InvalidType
		 in
		    Iq iq_type
	    | _ -> InvalidStanza xml
   in
      match event with
	 | Iq `InvalidType ->
	      begin try
		 let id = Xml.get_attr_s xml "id" in
		    if IdMap.mem id !idmap then
		       idmap := IdMap.remove id !idmap
	      with Not_found -> ()
	      end;
	      Printf.printf "Invalid Stanza: %s\n" (Xml.element_to_string xml);
	      process_xml next_xml out
	 | InvalidStanza xml ->
	      Printf.printf "Invalid Stanza: %s\n" (Xml.element_to_string xml);
	      process_xml next_xml out
	 | _ ->
	      Muc_log.process_log event from xml;
	      let () =
		 try
		    List.iter (fun proc -> proc event from xml out) !filters;
		 with 
		    | FilteredOut ->
			 process_xml next_xml out
		    | exn -> print_exn exn ~xml
	      in
		 begin match event with
		    | Iq _ ->
			 process_iq event from xml out
		    | MUC_message _
		    | Message ->
			 process_message event from xml out;
		    | _ -> 
			 List.iter (fun proc -> 
				       try proc event from xml out with exn ->
					  print_exn exn ~xml
				   ) !catchset
		 end;
		 process_xml next_xml out

let quit out =
   List.iter (fun proc -> try proc out with exn -> print_exn exn) !onquit;
   Pervasives.exit 0

let check_access (jid:jid) classname =
   let who =
      try 
	 let env = GroupchatMap.find (jid.luser, jid.lserver) !groupchats in
	 let nick = jid.lresource in
	 let item = Nicks.find nick env.nicks in
	    item.jid
      with Not_found -> jid 
   in
   let acls = get_subels Config.config ~tag:"acl" in
      if List.exists (fun a -> 
			 let jid = jid_of_string (get_attr_s a "jid") in
			    if jid.luser = who.luser && 
			       jid.lserver = who.lserver &&
			       get_attr_s a "class" = classname then
				  true else false) acls 
      then true else false
