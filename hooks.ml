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
			    out (Muc.join_room env.mynick room)) !groupchats
   in
      ref [on_start]

let onquit = ref []
let catchset = ref []
let filters = ref []

type reg_handle =
   | Xmlns of string * (xmpp_event -> element -> (element -> unit) -> unit)
   | Id of string * (xmpp_event -> element -> (element -> unit) -> unit)
   | Command of string * 
	(string -> xmpp_event -> element -> (element -> unit) -> unit)
   | OnStart of ((element -> unit) -> unit)
   | OnQuit of ((element -> unit) -> unit)
   | Catch of (xmpp_event -> element -> (element -> unit) -> unit)
   | Filter of (xmpp_event -> element -> (element -> unit) -> unit)
   | PresenceHandle of (xmpp_event -> element -> (element -> unit) -> unit)

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

let process_iq event xml out =
   let id = safe_get_attr_s xml "id" in
      if id <> "" then
	 match event with
	    | Iq `Result
	    | Iq `Error ->
		 begin try
		    let f = IdMap.find id !idmap in
		       f event xml out;
		       idmap := IdMap.remove id !idmap
		 with _ -> ()
		 end
	    | Iq `Get
	    | Iq `Set ->
		 begin try
		    let f = XmlnsMap.find (get_xmlns xml) !xmlnsmap in
		       f event xml out;
		 with _ -> ()
		 end
	    | _ -> ()
      else 
	 match event with
	    | Iq `Get
	    | Iq `Set ->
		 begin try
		    let f = XmlnsMap.find (get_xmlns xml) !xmlnsmap in
		       f event xml out;
		 with _ -> ()
		 end
	    | _ -> ()

let do_command text event xml out =
   let word = 
      try String.sub text 0 (String.index text ' ') with Not_found -> text in
   let f = CommandMap.find word !commands in
   let text = try string_after text (String.index text ' ') with _ -> "" in
      f (trim text) event xml out

let process_message event xml out =
   match event with
      | MUC_message (room, msg_type, author, nick, text) ->
	   if msg_type <> `Error then
	      if text <> "" then
		 try
		    let room_env = GroupchatMap.find room !groupchats in
		       begin match msg_type with
			  | `Groupchat ->
			       if author <> room_env.mynick && nick = "" then
				  do_command text event xml out
			       else
				  raise Not_found
			  | _ ->
			       do_command text event xml out
		       end
		 with Not_found ->
		    List.iter  (fun f -> f event xml out) !catchset
	      else
		 List.iter  (fun f -> f event xml out) !catchset
      | Message ->
	   let text = try get_cdata xml ~path:["body"] with Not_found -> "" in
	      try do_command text event xml out with Not_found ->
		 List.iter  (fun f -> f event xml out) !catchset

exception FilteredOut
exception InvalidStanza of string
  
let rec process_xml next_xml out =
   let xml = next_xml () in
   let from = get_attr_s xml "from" in
   let room = get_bare_jid from in
   let tag = get_tagname xml in
   let event =
      if GroupchatMap.mem room !groupchats then
	 let nick = get_resource from in
	    match tag with
	       | "presence" ->
		    Muc.process_presence room nick xml out
	       | "message" ->
		    Muc.process_message room nick xml out
	       | "iq" ->
		    let iq_type = match safe_get_attr_s xml "type" with
		       | "get" -> `Get
		       | "set" -> `Set
		       | "result" -> `Result
		       | "error" -> `Error
		    in
		       Iq iq_type
	       | _ -> raise (InvalidStanza (Xml.element_to_string xml))
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
		 in
		    Iq iq_type
	    | _ -> raise (InvalidStanza (Xml.element_to_string xml))
   in
   let () =
      Muc_log.process_log event xml;
      try
	 List.iter (fun proc -> proc event xml out) !filters;
      with FilteredOut ->
	 process_xml next_xml out
   in
      begin match event with
	 | Iq _ ->
	      process_iq event xml out
	 | MUC_message _
	 | Message ->
	      process_message event xml out;
	 | _ -> 
	      List.iter (fun proc -> proc event xml out) !catchset
      end;
      process_xml next_xml out

let quit out =
   List.iter (fun proc -> proc out) !onquit;
   Pervasives.exit 0

let check_access jid classname =
   let who =
      let room = get_bare_jid jid in
	 try 
	    let env = GroupchatMap.find room !groupchats in
	    let nick = get_resource jid in
	    let item = Nicks.find nick env.nicks in
	       get_bare_jid (item.jid)
	 with Not_found -> get_bare_jid jid 
   in
   let acls = get_subels Config.config ~tag:"acl" in
      if List.exists (fun a -> 
			 if get_attr_s a "jid" = who &&
			    get_attr_s a "class" = classname then
			       true else false) acls 
      then true else false
