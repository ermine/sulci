(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xmpp
open Jid

let timerQ = Scheduler.create ()
let _ = Scheduler.run timerQ

let new_id = 
  let my_id = ref 0 in
    fun () ->
      incr my_id;
      "stoat_" ^ string_of_int !my_id

type id = string

exception BadEntity

type entity =
  | EntityMe
  | EntityYou
  | EntityUser
  | EntityHost

type local_env = {
  env_groupchat: bool;
  env_lang: string;
  env_check_access: Jid.jid -> string -> bool;
  env_get_entity: string -> Jid.jid -> (entity * Jid.jid)
}

module Id =
struct
  type t = string
  let compare = compare
end

type xmpp_event =
  | Message
  | Iq of string * iq_type * string
  | Presence

exception Filtered
