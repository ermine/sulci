(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open Xmpp
open Jid
open Jeps
open Types
open Common
open Hooks

exception NoError
  
let process_error xml env entity from text =
  let cond,_,_,_ = Error.parse_error xml in
    match cond with
      | `ERR_FEATURE_NOT_IMPLEMENTED ->
          (match entity with
             | EntityHost ->
                 Lang.get_msg env.env_lang
                   "error_server_feature_not_implemented" [text]
             | _ ->
                 Lang.get_msg env.env_lang
                   "error_client_feature_not_implemented" [text])
      | `ERR_REMOTE_SERVER_TIMEOUT ->
          Lang.get_msg env.env_lang "error_remote_server_timeout" [from.ldomain]
      | `ERR_REMOTE_SERVER_NOT_FOUND ->
          Lang.get_msg env.env_lang
            "error_remote_server_not_found" [from.ldomain]
      | `ERR_SERVICE_UNAVAILABLE ->
          (match entity with
             | EntityHost ->
                 Lang.get_msg env.env_lang "error_server_service_unavailable" 
                   [text]
             | EntityYou ->
                 Lang.get_msg env.env_lang"error_your_service_unavailable" []
             | _ ->
                 Lang.get_msg env.env_lang "error_client_service_unavailable" 
                   [text]
          )
      | `ERR_RECIPIENT_UNAVAILABLE ->
          Lang.get_msg env.env_lang "error_recipient_unavailable" [text]
      | `ERR_NOT_ALLOWED ->
          Lang.get_msg env.env_lang "error_not_allowed" [text]
            
      | `ERR_BAD_REQUEST
      | `ERR_CONFLICT
      | `ERR_FORBIDDEN
      | `ERR_GONE
      | `ERR_INTERNAL_SERVER_ERROR
      | `ERR_ITEM_NOT_FOUND
      | `ERR_JID_MALFORMED
      | `ERR_NOT_ACCEPTABLE
      | `ERR_NOT_AUTHORIZED
      | `ERR_PAYMENT_REQUIRED
      | `ERR_REDIRECT
      | `ERR_REGISTRATION_REQUIRED
      | `ERR_RESOURCE_CONSTRAINT
      | `ERR_SUBSCRIPTION_REQUIRED
      | `ERR_UNDEFINED_CONDITION
      | `ERR_UNEXPECTED_REQUEST ->
          try 
            get_cdata ~path:["error"; "text"] xml
          with _ ->
            Lang.get_msg env.env_lang "error_any_error" [text]
              
let simple_query_entity ?me ?(error_exceptions=[]) success
    ?query_subels ?query_tag xmlns
    text from xml env out =
  try
    let entity, entity_jid = env.env_get_entity text from in
      match entity, me with
        | EntityMe, Some f ->
            f text from xml env out
        | _, _ ->
            let proc t f x o =
              match t with
                | `Result ->
                    make_msg o xml (success text entity env x)
                | `Error -> (
                    try
                      if error_exceptions <> [] then (
                        let cond,_,_,_ = Error.parse_error x in
                          if List.mem cond error_exceptions then
                            raise NoError;
                      );
                      make_msg o xml 
                        (process_error x env entity f
                           (if text = "" then
                              if env.env_groupchat && entity = EntityYou then
                                f.resource
                              else
                                f.string
                            else
                              text))
                    with NoError ->
                      make_msg o xml (success text entity env x)
                  )
                | _ -> ()
            in
            let id = new_id () in
              register_iq_query_callback id proc;
              out (make_iq ~to_:(string_of_jid entity_jid)
                     ~id ~type_:`Get ?query_tag ~xmlns 
                     ?subels:query_subels ())
  with _ ->
    make_msg out xml (Lang.get_msg env.env_lang "invalid_entity" [])
        
let _ =
  Hooks.register_handle 
    (Xmlns ("jabber:iq:version", 
            (fun event from xml out -> 
               match event with
                 | Iq (id, type_, xmlns) ->
                     if type_ = `Get && xmlns = "jabber:iq:version" then
                       out (iq_version_reply 
                              Version.name Version.version xml)
                     else
                       ()
                 | _ -> ())))
    (* "jabber:iq:last", Iq.iq_last_reply *)
    
    
