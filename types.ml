
let my_id = ref 0

let new_id () = 
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
   jid: string;
   status: string;
   show: string;
   role: string;
   orig_nick: string;
   affiliation: string
}

type groupchat_t = {
   mynick: string;
   lang: string;
   nicks: participant_t Nicks.t;
}

module GroupchatMap = Map.Make(Id)
let groupchats = ref (GroupchatMap.empty:groupchat_t GroupchatMap.t)

type msg_type = [`Groupchat | `Chat | `Normal | `Error]
type iq_type = [`Get | `Set | `Result | `Error]

type xmpp_event = 
   | MUC_join of string * string * participant_t
   | MUC_leave of string* string * string * participant_t
   | MUC_change_nick of string * string * string * participant_t
   | MUC_kick of string * string * string * participant_t
   | MUC_ban of string * string * string * participant_t
   | MUC_presence of string * string * participant_t
   | MUC_topic of string * string * string
   | MUC_message of string * msg_type * string * string * string
   | MUC_other of string
   | MUC_history of string
   | Message
   | Iq of iq_type
   | Presence
