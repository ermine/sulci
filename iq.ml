(*
 * (c) 2004-2012 Anastasia Gornostaeva
 *)

open JID
open StanzaError
open Common
open Hooks

open XMPPClient

let get_host = function
  | EntityMe jid
  | EntityYou jid
  | EntityHost jid
  | EntityUser (_, jid) ->
      jid.domain
      
let process_error error env entity =
  match error.err_condition with
    | ERR_FEATURE_NOT_IMPLEMENTED -> (
        match entity with
          | EntityHost host ->
              Lang.get_msg env.env_lang "error_server_feature_not_implemented"
                [host.ldomain]
          | EntityMe _ ->
              Lang.get_msg env.env_lang
                "error_my_client_feature_not_implemented" []
          | EntityYou jid ->
              Lang.get_msg env.env_lang
                "error_your_client_feature_not_implemented" []
          | EntityUser (text, jid) ->
              Lang.get_msg env.env_lang "error_client_feature_not_implemented"
                [text]
      )
    | ERR_REMOTE_SERVER_TIMEOUT ->
        Lang.get_msg env.env_lang "error_remote_server_timeout" [get_host entity]
    | ERR_REMOTE_SERVER_NOT_FOUND ->
        Lang.get_msg env.env_lang "error_remote_server_not_found"
          [get_host entity]
    | ERR_SERVICE_UNAVAILABLE ->
        (match entity with
           | EntityHost host ->
               Lang.get_msg env.env_lang "error_server_service_unavailable" 
                 [host.domain]
           | EntityYou _jid ->
               Lang.get_msg env.env_lang"error_your_service_unavailable" []
           | EntityMe jid ->
               Lang.get_msg env.env_lang "error_my_service_unavailable" []
           | EntityUser (text, jid) ->
               Lang.get_msg env.env_lang "error_client_service_unavailable" 
                 [text]
        )
    | ERR_RECIPIENT_UNAVAILABLE ->
        Lang.get_msg env.env_lang "error_recipient_unavailable" []
    | ERR_NOT_ALLOWED ->
        Lang.get_msg env.env_lang "error_not_allowed" []
          
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
          Lang.get_msg env.env_lang "error_any_error" []
        else
          error.err_text
    | UNKNOWN_CONDITION other ->
        other
            
let simple_query_entity ?me ?(error_exceptions=[]) success
    ~payload xmpp env kind jid_from text =
  let entity =
    try Some (env.env_get_entity text jid_from)
    with _ -> None in
    match entity with
      | None ->
        env.env_message xmpp kind jid_from
          (Lang.get_msg env.env_lang "invalid_entity" [])
      | Some e ->
        match e, me with
          | EntityMe _, Some f ->
            f xmpp env kind jid_from text
          | EntityMe jid, _
          | EntityYou jid, _
          | EntityUser (_, jid), _
          | EntityHost jid, _ ->
            let proc ev _jidfrom _jidto _lang () =
              match ev with
                | IQResult el ->
                  env.env_message xmpp kind jid_from
                    (success env text e el)
                | IQError err -> (
                  if List.mem err.err_condition error_exceptions then
                    env.env_message xmpp kind jid_from
                      (success env text e None)
                  else
                    env.env_message xmpp kind jid_from
                      (process_error err env e)
                )
            in
              XMPPClient.make_iq_request xmpp ~jid_to:jid (IQGet payload) proc
        
let simple_query_entity2 ?me ?(error_exceptions=[]) success get
    xmpp env kind jidfrom text =
  let entity =
    try Some (env.env_get_entity text jidfrom)
    with _ -> None in
    match entity with
      | None ->
        env.env_message xmpp kind jidfrom
          (Lang.get_msg env.env_lang "invalid_entity" [])
      | Some entity ->
        match entity, me with
          | EntityMe _, Some f ->
            f xmpp env kind jidfrom text
          | EntityMe jid, _
          | EntityYou jid, _
          | EntityUser (_, jid), _
          | EntityHost jid, _ ->
            let proc ?jid_from ?jid_to ?lang answer =
              env.env_message xmpp kind jidfrom (success env text entity answer)
            in
            let error_callback err =
              if List.mem err.err_condition error_exceptions then
                env.env_message xmpp kind jidfrom (success env text entity None)
              else
                env.env_message xmpp kind jidfrom (process_error err env entity)
            in
              get xmpp ?jid_from:None ?jid_to:(Some jid) ?lang:None
                ?error_callback:(Some error_callback) proc
                
    
  
let os = (let f = Unix.open_process_in "uname -sr" in
          let answer = input_line f in
            ignore (Unix.close_process_in f); answer)
  
let features xmpp =
  let module XV = XEP_version.Make(XMPPClient) in
    XMPPClient.register_iq_request_handler xmpp XV.ns_version
      XV.(
        iq_request ~get:(fun ?jid_from ?jid_to ?lang () -> {
          name = Version.name;
          version = Version.version;
          os = os
        })
      )
