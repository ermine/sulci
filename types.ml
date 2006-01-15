(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xmpp

let _ = Scheduler.init ()

exception InvalidJID

let new_id = 
   let my_id = ref 0 in
      fun () ->
	 incr my_id;
	 "stoat_" ^ string_of_int !my_id

module Id =
struct
   type t = string
   let compare = compare
end

(* groupchat *)
module Nicks =
struct
   type participant_t = {
      jid: Xmpp.jid option;
      status: string;
      show: presence_show_t;
      role: string;
      orig_nick: string;
      affiliation: string
   }
   type t = (string * participant_t) list
   let find nick nicks = List.assoc nick nicks
   let add nick item nicks =
      let len = String.length nick in
      let rec aux_add tail acc =
	 match tail with
	    | [] -> List.rev ((nick, item) :: acc)
	    | (nick1, item1) as x :: xs ->
		 if String.length nick1 > len then
		    aux_add xs (x :: acc)
		 else
		    List.rev ((nick, item) :: acc) @ tail
      in
	 aux_add nicks []
   let remove nick nicks = List.remove_assoc nick nicks
   let mem nick nicks = List.mem_assoc nick nicks
   let iter = List.iter
end

type groupchat_t = {
   mynick: string;
   lang: string;
   nicks: Nicks.t
}

type room = string * string

module GID =
struct
   type t = room
   let compare = compare
end

module GroupchatMap = Map.Make(GID)
let groupchats = ref (GroupchatMap.empty:groupchat_t GroupchatMap.t)

type xmpp_event = 
   | MUC_join of Nicks.participant_t
   | MUC_leave of string * Nicks.participant_t
   | MUC_change_nick of string * Nicks.participant_t
   | MUC_kick of string * Nicks.participant_t
   | MUC_ban of string * Nicks.participant_t
   | MUC_presence of Nicks.participant_t
   | MUC_topic of string
   | MUC_message of message_type * string * string
   | MUC_other
   | MUC_history
   | Message
   | Iq of string * iq_type * string
   | Presence
