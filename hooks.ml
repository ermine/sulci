(*
 * (c) 2004-2010 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open XMPP
open JID
open Common
open Lang
  
let _ = Printexc.record_backtrace true

exception BadEntity

type xmpp = session_data XMPP.t
and session_data = {
  mutable max_stanza_length : int;
  mutable max_message_length : int;
  mutable on_connect : (xmpp -> unit) list;
  mutable on_disconnect : (unit -> unit) list;
  mutable presence_hooks : presence_content hook list;
  mutable message_hooks : message_content hook list;
  skey : string
}
and 'a hook = {
  priority : int;
  name : string;
  proc : xmpp -> env -> 'a stanza -> 'a hook list -> unit
}
and env = {
  env_identity : jid -> jid;
  env_lang: string;
  env_get_entity: string -> JID.jid -> entity;
  env_message : xmpp -> message_type option -> jid -> ?response_tail:string ->
                                               string -> unit;
}
and entity =
  | EntityMe of JID.jid
  | EntityYou of JID.jid
  | EntityUser of string * JID.jid
  | EntityHost of JID.jid

let catch f x = try Some (f x) with Not_found -> None

let log = new Logger.logger ()

type opts = (string * (string * string) list) list
    
let processors : (opts -> xmpp -> unit) list ref = ref []

let add_for_token proc =
  processors := proc :: !processors
    
let run_for_token opts xmpp =
  List.iter (fun proc -> proc opts xmpp) (List.rev !processors)

let add_presence_hook xmpp priority name proc =
  xmpp.data.presence_hooks <-
    List.fast_sort (fun h1 h2 -> compare h1.priority h2.priority)
    ({priority = priority; name = name; proc = proc} :: xmpp.data.presence_hooks)

let add_message_hook xmpp priority name proc =
  xmpp.data.message_hooks <-
    List.fast_sort (fun h1 h2 -> compare h1.priority h2.priority)
    ({priority = priority; name = name; proc = proc} :: xmpp.data.message_hooks)

let register_on_connect xmpp proc =
  xmpp.data.on_connect <- proc :: xmpp.data.on_connect

let register_on_disconnect xmpp proc =
  xmpp.data.on_disconnect <- proc :: xmpp.data.on_disconnect

let get_entity text jid_from =
  if text = "" then
    EntityYou jid_from
  else
    let jid = try JID.of_string text with _ -> raise BadEntity in
      if jid.lnode = "" then (
        (try dnsprep jid.ldomain;
         with _ -> raise BadEntity);
        EntityHost jid
      ) else if jid_from.lnode = jid.lnode && jid_from.ldomain = jid.ldomain then
        EntityYou jid
      else
        EntityUser (text, jid)

let make_msg xmpp kind jid_to ?response_tail response =
  let tail =
    match response_tail with
      | None -> ""
      | Some t -> "\n" ^ t
  in
  let max_message_length = xmpp.data.max_message_length in
  let msgs = split_long_message max_message_length response tail in
    List.iter (fun body ->
                 XMPP.send_message xmpp ?kind ~jid_to ~body ()
              ) msgs

let add_tmp_hook hooks name hook =
  hooks @ [{priority = -1; name = name; proc = hook}]

let do_hook xmpp env stanza hooks =
  if hooks <> [] then
    let hook = List.hd hooks in
      hook.proc xmpp env stanza (List.tl hooks)

let message_error xmpp ?id ?jid_from ?jid_to ?lang error =
  ()

let message_callback xmpp stanza =
  let env = { env_identity = (fun jid -> jid);
              env_lang = Lang.get_lang stanza.lang;
              env_get_entity = get_entity;
              env_message = make_msg;
            } in
    do_hook xmpp env stanza xmpp.data.message_hooks
            
let presence_error xmpp ?id ?jid_from ?jid_to ?lang error =
  ()

let presence_callback xmpp stanza =
  let env = { env_identity = (fun jid -> jid);
              env_lang = Lang.get_lang stanza.lang;
              env_get_entity = get_entity;
              env_message = make_msg;
            } in
    do_hook xmpp env stanza xmpp.data.presence_hooks
          
