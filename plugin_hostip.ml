(*
 * (c) 2006-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Unix
open Types
open Common
open Hooks
open Http_suck
open Netconversion

let rex = Pcre.regexp "^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$"

let hostip text from xml env out =
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
          make_msg out xml
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
                        "\n" ^ Xml.encode resp
                    with exn ->
                      Lang.get_msg env.env_lang "conversation_trouble" []
                  )
                | _ ->
                    Lang.get_msg env.env_lang "plugin_hostip_failed" []
            in
              make_msg out xml response
          in
            Http_suck.http_get url callback
              
let _ =
  register_command "hostip" hostip
