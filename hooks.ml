(*                                                                          *)
(* (c) 2004, Anastasia Gornostaeva. <ermine@ermine.pp.ru>                   *)
(*                                                                          *)

open Xml
open Xmpp
open Common

let my_id = ref 0

let new_id () = 
   incr my_id;
   "stoat_" ^ string_of_int !my_id

type hook_t = HookFrom of string | HookId of string | HookXmlns of string
     
module XId =
struct
   type t = hook_t
   let compare = compare
end
module HookMap = Map.Make(XId)
let hooks = ref HookMap.empty

module Id =
struct
   type t = string
   let compare = compare
end

module CommandMap = Map.Make(Id)
let commands = ref CommandMap.empty

let onstart = ref []
let onquit = ref []
let catchset = ref []

(* groupchat *)
module Nicks = Map.Make(Id)

type groupchat_t = {
   mynick: string;
   lang: string;
   nicks: string Nicks.t;
}

module GroupchatMap = Map.Make(Id)
let groupchats = ref (GroupchatMap.empty:groupchat_t GroupchatMap.t)



type reg_handle =
   | From of string * (element -> (element -> unit) -> unit)
   | Xmlns of string * (element -> (element -> unit) -> unit)
   | Id of string * (element -> (element -> unit) -> unit)
   | Command of string * (string -> element -> (element -> unit) -> unit)
   | OnStart of ((element -> unit) -> unit)
   | OnQuit of ((element -> unit) -> unit)
   | Catch of (element -> (element -> unit) -> unit)

let register_handle (handler:reg_handle) =
   match handler with
      | From (from, proc) ->
	   hooks := HookMap.add (HookFrom from) proc !hooks
      | Xmlns (xmlns, proc) ->
	   hooks := HookMap.add (HookXmlns xmlns) proc !hooks
      | Id (id, proc) ->
	   hooks := HookMap.add (HookId id) proc !hooks
      | Command (command, proc) ->
	   commands := CommandMap.add command proc !commands
      | OnStart proc ->
	   onstart := proc :: !onstart;
      | OnQuit proc ->
	   onquit := proc :: !onquit;
      | Catch proc ->
	   catchset := proc :: !catchset

let rec process_xml next_xml out =
   let xml = next_xml () in
   let () = 
      try
	 let f = HookMap.find 
		    (HookFrom (get_bare_jid (get_attr_s xml "from"))) !hooks in
            f xml out
      with _ -> () in
   let () = match get_tagname xml with
      | "iq" ->
	   if (safe_get_attr_s xml "type" = "result") ||
	      (safe_get_attr_s xml "type" = "error") &&
	      safe_get_attr_s xml "id" <> "" then
		 try
		    let f = 
		       HookMap.find (HookId (get_attr_s xml "id")) !hooks
		    in
		       f xml out;
		       hooks := HookMap.remove 
			  (HookId (get_attr_s xml "id")) !hooks
		 with _ -> ()
	   else if safe_get_attr_s xml "type" = "get" then begin
	      try
		 let f = HookMap.find (HookXmlns (get_xmlns xml)) !hooks in
		    f xml out;
	      with _ -> ()
	   end
      | "message" ->
	   if not (mem_xml xml ["message"] "x" ["xmlns", "jabber:x:delay"]) &&
	      not (safe_get_attr_s xml "type" = "error") then
		 let body = 
		    try skip_ws (get_cdata xml ~path:["body"]) 
		    with _ -> "" in
		    if body <> "" then
		       (try
			   let word = 
			      try
				 String.sub body 0 (String.index body ' ')
			      with Not_found -> body
			   in
			      if word.[String.length word - 1] <> ':' then
				 let f = CommandMap.find word !commands in
				 let text =
				    try 
				       string_after body (String.index body ' ')
				    with Not_found -> ""
				 in
				    f (trim text) xml out
			      else
				 raise Not_found
			with Not_found ->
			   List.iter  (fun f -> f xml out) !catchset)
      | _ -> ()
   in
      process_xml next_xml out

let quit out =
   List.iter (fun proc -> proc out) !onquit;
   Pervasives.exit 0
