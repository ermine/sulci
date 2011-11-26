(*
 * (c) 2006-2010 Anastasia Gornostaeva
 *)

open Unix
open Netconversion
open Common
open Hooks
open Plugin_command
open Http_suck

let rex = Pcre.regexp "^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$"

let hostip xmpp env kind jid_from text =
  let ip =
    try
      let h = gethostbyname text in
        Some (string_of_inet_addr h.h_addr_list.(0))
    with Not_found ->
      if Pcre.pmatch ~rex text then
        Some text
      else
        None
  in
    match ip with
      | None ->
          env.env_message xmpp kind jid_from
            (Lang.get_msg env.env_lang "plugin_hostip_bad_syntax" []);
      | Some ip ->
          let url = Printf.sprintf 
            "http://api.hostip.info/get_html.php?ip=%s&position=true" 
            ip in
          let callback data =
            let response = 
              match data with
                | OK (_media, charset, content) -> (
                    try
                      let enc = 
                        match charset with
                          | None -> `Enc_iso88591
                          | Some v -> encoding_of_string v
                      in
                      let resp =
                        if enc <> `Enc_utf8 then
                          convert ~in_enc:enc ~out_enc:`Enc_utf8 content
                        else
                          content
                      in
                        "\n" ^ resp
                    with _exn ->
                      Lang.get_msg env.env_lang "conversation_trouble" []
                  )
                | Exception _ ->
                    Lang.get_msg env.env_lang "plugin_hostip_failed" []
            in
              env.env_message xmpp kind jid_from response
          in
            Http_suck.http_get url callback
              
let plugin opts =
  ()
    
let _ =
  Plugin.add_plugin "hostip" plugin
