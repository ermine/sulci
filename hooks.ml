(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open XMPP
open Jid
open Common
open Lang
  
let _ = Printexc.record_backtrace true

exception PluginError of string
exception Filtered
exception BadEntity

type xmpp = string XMPP.t

type entity =
  | EntityMe of Jid.jid
  | EntityYou of Jid.jid
  | EntityUser of string * Jid.jid
  | EntityHost of Jid.jid

type env = {
  env_groupchat: bool;
  env_lang: string;
  env_get_entity: string -> Jid.jid -> entity;
  env_message :
    xmpp -> message_type option -> jid -> ?response_tail:string ->
                                               string -> unit;
}

type ('a, 'b) hook = {
  priority : int;
  name : string;
  proc : xmpp -> env -> ('a, 'b) stanza -> ('a, 'b) hook list -> unit
}

type global = {
  mutable max_message_length : int;
  mutable acls : (jid * string) list;
  mutable plugins :
    (string * ((string * (string * string) list) list -> unit)) list;
  mutable on_connect : (xmpp -> unit) list;
  mutable on_disconnect : (unit -> unit) list;
  mutable presence_hooks : (presence_type, presence_content) hook list;
  mutable message_hooks : (message_type, message_content) hook list;
}  

let global = {
  max_message_length = 1000;
  acls = [];
  plugins = [];
  on_connect = [];
  on_disconnect = [];
  presence_hooks = [];
  message_hooks = []
}

let catch f x = try Some (f x) with Not_found -> None

let log = new Logger.logger ()

let add_plugin (name:string) (proc:(string * (string * string) list) list -> unit) =
  global.plugins <- (name, proc) :: global.plugins

let add_presence_hook priority name proc =
  global.presence_hooks <-
    List.fast_sort (fun h1 h2 -> compare h1.priority h2.priority)
    ({priority = priority; name = name; proc = proc} :: global.presence_hooks)

let add_message_hook priority name proc =
  global.message_hooks <-
    List.fast_sort (fun h1 h2 -> compare h1.priority h2.priority)
    ({priority = priority; name = name; proc = proc} :: global.message_hooks)

let register_on_connect proc =
  global.on_connect <- proc :: global.on_connect

let register_on_disconnect proc =
  global.on_disconnect <- proc :: global.on_disconnect

let check_access jid classname =
  if classname = "" then
    true
 else
   List.exists (fun (jid', name) ->
                  (name = classname &&
                      jid'.lnode = jid.lnode && jid'.ldomain = jid.ldomain)
               ) global.acls

let get_entity text from =
  if text = "" then
    EntityYou from
  else
    let jid = try jid_of_string text with _ -> raise BadEntity in
      if jid.lnode = "" then (
        (try dnsprep jid.ldomain;
         with _ -> raise BadEntity);
        EntityHost jid
      ) else if from.lnode = jid.lnode && from.ldomain = jid.ldomain then
        EntityYou jid
      else
        EntityUser (text, jid)

let make_msg xmpp kind jid_to ?response_tail response =
  let tail =
    match response_tail with
      | None -> ""
      | Some t -> "\n" ^ t
  in
  let msgs = split_long_message global.max_message_length response tail in
    List.iter (fun body ->
                 XMPP.send_message xmpp ?kind ~jid_to ~body ()
              ) msgs

let do_hook xmpp env stanza hooks =
  if hooks <> [] then
    let hook = List.hd hooks in
      hook.proc xmpp env stanza (List.tl hooks)

let message_error xmpp ?id ?jid_from ?jid_to ?lang error =
  ()

let message_callback xmpp stanza =
  let env = { env_groupchat = false;
              env_lang = Lang.get_lang stanza.lang;
              env_get_entity = get_entity;
              env_message = make_msg;
            } in
    do_hook xmpp env stanza global.message_hooks
            
let presence_error xmpp ?id ?jid_from ?jid_to ?lang error =
  ()

let presence_callback xmpp stanza =
  let env = { env_groupchat = false;
              env_lang = Lang.get_lang stanza.lang;
              env_get_entity = get_entity;
              env_message = make_msg;
            } in
    do_hook xmpp env stanza global.presence_hooks
          
