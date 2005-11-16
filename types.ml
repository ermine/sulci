(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xmpp

let _ = Scheduler.init ()

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
(* bad place here, but unfortunatelly... *)
module Nicks = Map.Make(Id)

type participant_t = {
   jid: Xmpp.jid option;
   status: string;
   show: presence_show_t;
   role: string;
   orig_nick: string;
   affiliation: string
}

type groupchat_t = {
   mynick: string;
   lang: string;
   nicks: participant_t Nicks.t;
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
   | MUC_join of participant_t
   | MUC_leave of string * participant_t
   | MUC_change_nick of string * participant_t
   | MUC_kick of string * participant_t
   | MUC_ban of string * participant_t
   | MUC_presence of participant_t
   | MUC_topic of string
   | MUC_message of message_type * string * string
   | MUC_other
   | MUC_history
   | Message
   | Iq of string * iq_type * string
   | Presence
