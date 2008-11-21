(*
 * (c) 2004-2008 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open Xmpp
open Jid
open Jeps
open Common
open Hooks
open Types

let process_error xml lang entity from text =
  let cond,_,_,_ = Error.parse_error xml in
    match cond with
      | `ERR_FEATURE_NOT_IMPLEMENTED ->
          (match entity with
             | `Host _ ->
                 Lang.get_msg ~lang 
                   "error_server_feature_not_implemented" [text]
             | _ ->
                 Lang.get_msg ~lang 
                   "error_client_feature_not_implemented" [text])
      | `ERR_REMOTE_SERVER_TIMEOUT ->
          Lang.get_msg ~lang "error_remote_server_timeout" [from.ldomain]
      | `ERR_REMOTE_SERVER_NOT_FOUND ->
          Lang.get_msg ~lang "error_remote_server_not_found" [from.ldomain]
      | `ERR_SERVICE_UNAVAILABLE ->
          (match entity with
             | `Host _ ->
                 Lang.get_msg ~lang "error_server_service_unavailable" 
                   [text]
             | `You ->
                 Lang.get_msg ~lang"error_your_service_unavailable" []
             | _ ->
                 Lang.get_msg ~lang "error_client_service_unavailable" 
                   [text]
          )
      | `ERR_RECIPIENT_UNAVAILABLE ->
          Lang.get_msg ~lang "error_recipient_unavailable" [text]
      | `ERR_NOT_ALLOWED ->
          Lang.get_msg ~lang "error_not_allowed" [text]
            
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
            Lang.get_msg ~lang "error_any_error" [text]
              
let simple_query_entity ?me ?entity_to_jid success ?query_subels
    ?query_tag xmlns =
  let fun_entity_to_jid =
    match entity_to_jid with
      | Some f -> f
      | None ->
          fun entity event from ->
            match entity with
              | `Mynick mynick ->
                  string_of_jid {from with resource = mynick}
              | `You ->
                  string_of_jid from
              | `User user ->
                  if user.lresource = "" then
                    raise BadEntity
                  else
                    user.string
              | `Nick nick ->
                  string_of_jid {from with resource = nick}
              | `Host host ->
                  host.domain
              | _ ->
                  raise BadEntity
  in
    fun text event from xml (out:element -> unit) ->
      try
        let entity = get_entity text event from in
          match entity, me with
            | `Mynick _, Some f ->
                f text event from xml out
            | _, _ ->
                let to_ = fun_entity_to_jid entity event from in
                let lang = Lang.get_lang xml in
                let proc e f x o =
                  match e with
                    | Iq (_, `Result, _) ->
                        make_msg o xml 
                          (success text entity lang x)
                    | Iq (_, `Error, _) ->
                        make_msg o xml 
                          (process_error x lang entity f
                             (if text = "" then 
                                match event, entity with
                                  | MUC_message _, `You ->
                                      f.resource
                                  | _ ->
                                      f.string
                              else
                                text))
                    | _ -> ()
                in
                let id = new_id () in
                  register_handle (Id (id, proc));
                  out (make_iq ~to_ ~id ~type_:`Get ?query_tag ~xmlns 
                         ?subels:query_subels ())
      with _ ->
        make_msg out xml (Lang.get_msg ~xml "invalid_entity" [])
          
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
    
    
