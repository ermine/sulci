(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open XMPP
open Jid
open StanzaError
open Jeps
open Types
open Common
open Hooks

let process_error error env entity from text =
  match error.err_condition with
    | ERR_FEATURE_NOT_IMPLEMENTED -> (
        match entity with
          | EntityHost ->
              Lang.get_msg env.env_lang
                "error_server_feature_not_implemented" [text]
          | EntityMe
          | EntityYou
          | EntityUser ->
              Lang.get_msg env.env_lang
                "error_client_feature_not_implemented" [text])
    | ERR_REMOTE_SERVER_TIMEOUT ->
        Lang.get_msg env.env_lang "error_remote_server_timeout" [from.ldomain]
    | ERR_REMOTE_SERVER_NOT_FOUND ->
        Lang.get_msg env.env_lang
          "error_remote_server_not_found" [from.ldomain]
    | ERR_SERVICE_UNAVAILABLE ->
        (match entity with
           | EntityHost ->
               Lang.get_msg env.env_lang "error_server_service_unavailable" 
                 [text]
           | EntityYou ->
               Lang.get_msg env.env_lang"error_your_service_unavailable" []
           | EntityMe
           | EntityUser ->
               Lang.get_msg env.env_lang "error_client_service_unavailable" 
                 [text]
        )
    | ERR_RECIPIENT_UNAVAILABLE ->
        Lang.get_msg env.env_lang "error_recipient_unavailable" [text]
    | ERR_NOT_ALLOWED ->
        Lang.get_msg env.env_lang "error_not_allowed" [text]
    | UNKNOWN_CONDITION other ->
        other
          
    | ERR_BAD_REQUEST
    | ERR_CONFLICT
    | ERR_FORBIDDEN
    | ERR_GONE
    | ERR_INTERNAL_SERVER_ERROR
    | ERR_ITEM_NOT_FOUND
    | ERR_JID_MALFORMED
    | ERR_NOT_ACCEPTABLE
    | ERR_NOT_AUTHORIZED
    | ERR_PAYMENT_REQUIRED
    | ERR_REDIRECT
    | ERR_REGISTRATION_REQUIRED
    | ERR_RESOURCE_CONSTRAINT
    | ERR_SUBSCRIPTION_REQUIRED
    | ERR_UNDEFINED_CONDITION
    | ERR_UNEXPECTED_REQUEST ->
        if error.err_text = "" then
          Lang.get_msg env.env_lang "error_any_error" [text]
        else
          error.err_text
            
let simple_query_entity ?me ?(error_exceptions=[]) success
    ~payload text from xml env out =
  try
    let entity, entity_jid = env.env_get_entity text from in
      match entity, me with
        | EntityMe, Some f ->
            f text from xml env out
        | EntityMe, _
        | EntityYou, _
        | EntityUser, _
        | EntityHost, _ ->
            let proc t f x o =
              match t with
                | IqResult el ->
                    make_msg o xml (success text entity env el)
                | IqError err -> (
                    if List.mem err.err_condition error_exceptions then
                      make_msg o xml (success text entity env None)
                    else
                      make_msg o xml 
                        (process_error err env entity f
                           (if text = "" then
                              if env.env_groupchat && entity = EntityYou then
                                f.resource
                              else
                                f.string
                            else
                              text))
                    
                  )
                | _ -> ()
            in
            let id = new_id () in
              register_iq_query_callback id proc;
              out (make_iq ~ns:ns_client ~jid_to:(string_of_jid entity_jid)
                     ~id ~type_:`Get ~payload:[payload] ()) 
  with _ ->
    make_msg out xml (Lang.get_msg env.env_lang "invalid_entity" [])
        
let _ =
  Hooks.register_handle 
    (Xmlns (Some "jabber:iq:version", 
            (fun event _from xml out -> 
               match event with
                 | IqGet _el ->
                     out (iq_version_reply Version.name Version.version xml)
                 | _ -> ())))
    (* "jabber:iq:last", Iq.iq_last_reply *)
    
    
(*    
<iq from='chote.net' to='stoat@jabber.ru/2' type='error' id='stoat_2'>
      <query xmlns='jabber:iq:version'/>
      <error code='404' type='cancel'>
      <remote-server-not-found xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
      </error></iq>
*)
