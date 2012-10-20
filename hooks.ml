(*
 * (c) 2004-2012 Anastasia Gornostaeva
 *)

open JID
open Common
  
module UnitMonad =
struct
  type 'a t = 'a
  let return x = x
  let (>>=) v f = f v
  let fail exn = raise exn
  let catch f1 f2 = try f1 () with exn -> f2 exn
end    

module ID =
struct
  type t = string
  let compare = Pervasives.compare
end
module IDCallback =
struct
  module T = Treap.Map(ID)
  type 'a t = 'a T.t
  let empty = T.empty
  let add key value t = T.add t key value 1
  let remove key t = T.delete t key
  let find key t = fst (T.find t key)
end

module XMPPClient = XMPP.Make (UnitMonad) (Xmlstream.XmlStream) (IDCallback)
open XMPPClient

exception BadEntity

type xmpp = user_data XMPPClient.session_data
and user_data = {
  deflang : string;
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
  env_identity : JID.t -> JID.t;
  env_lang: string;
  env_get_entity: string -> JID.t -> entity;
  env_message : xmpp -> message_type option -> JID.t -> ?response_tail:string ->
                                               string -> unit;
}
and entity =
  | EntityMe of JID.t
  | EntityYou of JID.t
  | EntityUser of string * JID.t
  | EntityHost of JID.t

let log = new Logger.logger ()

type opts = (string * (string * string) list) list
    
let processors : (opts -> user_data -> unit) list ref = ref []

let add_for_token proc =
  processors := proc :: !processors
    
let run_for_token opts (user_data:user_data) =
  List.iter (fun proc -> proc opts user_data) (List.rev !processors)

let add_presence_hook user_data priority name proc =
  user_data.presence_hooks <-
    List.fast_sort (fun h1 h2 -> Pervasives.compare h1.priority h2.priority)
    ({priority = priority; name = name; proc = proc} ::
        user_data.presence_hooks)

let add_message_hook user_data priority name proc =
  user_data.message_hooks <-
    List.fast_sort (fun h1 h2 -> Pervasives.compare h1.priority h2.priority)
    ({priority = priority; name = name; proc = proc} ::
        user_data.message_hooks)

let register_on_connect user_data proc =
  user_data.on_connect <- proc :: user_data.on_connect

let register_on_disconnect user_data proc =
  user_data.on_disconnect <- proc :: user_data.on_disconnect

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
  let max_message_length = xmpp.user_data.max_message_length in
  let msgs = split_long_message max_message_length response tail in
    List.iter (fun body ->
      XMPPClient.send_message xmpp ?kind ~jid_to ~body ()
    ) msgs

let add_tmp_hook hooks name hook =
  hooks @ [{priority = -1; name = name; proc = hook}]

let do_hook xmpp env stanza hooks =
  if hooks <> [] then
    let hook = List.hd hooks in
      hook.proc xmpp env stanza (List.tl hooks)
        
let message_error xmpp ?id ?jid_from ?jid_to ?lang error =
  ()

let get_lang xmpp = function
  | None -> xmpp.user_data.deflang
  | Some v -> v

let message_callback xmpp stanza =
  let env = { env_identity = (fun jid -> jid);
              env_lang = get_lang xmpp stanza.lang;
              env_get_entity = get_entity;
              env_message = make_msg;
            } in
    do_hook xmpp env stanza xmpp.user_data.message_hooks
            
let presence_error xmpp ?id ?jid_from ?jid_to ?lang error =
  ()

let presence_callback xmpp stanza =
  let env = { env_identity = (fun jid -> jid);
              env_lang = get_lang xmpp stanza.lang;
              env_get_entity = get_entity;
              env_message = make_msg;
            } in
    do_hook xmpp env stanza xmpp.user_data.presence_hooks
          
