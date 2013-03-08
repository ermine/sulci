(*
 * (c) 2004-2013 Anastasia Gornostaeva
 *)

open Light_xml
open XMPP
open Hooks
open Common
open Plugin_command
open Http_suck

let blogs xmpp env kind jid_from text =
  if text = "" then
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_yandex_bad_syntax" [])
  else
    let callback data =
      let response_tail = ref None in
      let resp = match data with
        | OK (_media, _charset, content) -> (
          try
            let parsed = Conv_xml.parse_document content in
            let item = get_tag parsed ["channel"; "item"] in
            let title = Dehtml.html2text 
              (get_cdata item ~path:["title"]) in
            let pubdate = get_cdata item ~path:["pubDate"] in
            let link = get_cdata item ~path:["link"] in
            let descr = Dehtml.html2text 
              (get_cdata item ~path:["description"]) in
              response_tail := Some (link ^ "\n" ^ pubdate);
              Printf.sprintf "%s\n%s" (trim title) (trim descr)
          with Not_found ->
            Lang.get_msg env.env_lang "plugin_yandex_not_found" []
        )
        | Exception exn ->
          match exn with
            | ClientError ->
              Lang.get_msg env.env_lang "plugin_yandex_404" []
            | ServerError ->
                  Lang.get_msg env.env_lang "plugin_yandex_server_error" []
            | _ ->
              Lang.get_msg env.env_lang "plugin_yandex_server_error" []
      in
        env.env_message xmpp kind jid_from ?response_tail:!response_tail resp
    in
    let url = 
      "http://blogs.yandex.ru/search.rss?how=tm&rd=2&charset=UTF-8&numdoc=1&text="
      ^ Netencoding.Url.encode (decode text) in
      Http_suck.http_get url callback
        
let plugin opts =
  add_for_token
    (fun _opts xmpp ->
      add_commands xmpp [("blogs", blogs);
                        ] opts
    )

let () =
  Plugin.add_plugin "yandex" plugin
  
