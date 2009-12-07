(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
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

let traffic xmpp env kind jid_from text =
  let callback data =
    let resp = match data with
      | OK (_media, _charset, content) ->
          content
      | Exception exn ->
          match exn with
            | ClientError ->
                Lang.get_msg env.env_lang "plugin_yandex_404" []
            | ServerError ->
                Lang.get_msg env.env_lang "plugin_yandex_server_error" []
            | _ ->
                Lang.get_msg env.env_lang "plugin_yandex_server_error" []
    in
      env.env_message xmpp kind jid_from resp
  in
  let url = "http://export.yandex.ru/maps/traffic.xml" in
    Http_suck.http_get url callback

let try_assoc name fields = try List.assoc name fields with Not_found -> "n/a"
  
let parse_weather content =
  let parsed = parse_document content in
  let fields =
    List.fold_left (fun acc -> function
                      | Xmlelement (tag, _, _) as el ->
                          (tag, get_cdata el) :: acc
                      | Xmlcdata _ -> acc
                   ) [] (get_subels parsed) in
  let city = try_assoc "city" fields
  and country = try_assoc "country" fields
  and w_type = try_assoc "weather_type" fields
  and temperature = try_assoc "temperature" fields
  and pressure = try_assoc "pressure" fields
  and dampness = try_assoc "dampness" fields in
    Printf.sprintf "%s (%s) -- %s %s влажность: %s давление: %s"
      city country temperature w_type dampness pressure

let weather xmpp env kind jid_from text =
  if text = "" then
    env.env_message xmpp kind jid_from
      "Choice city code at http://weather.yandex.ru/27612/choose/"
  else
    let callback data =
      let resp = match data with
        | OK (_media, _charset, content) -> (
            try parse_weather content
            with exn ->
              log#error "plugin_yandex[weather]: %s" (Printexc.to_string exn);
              "Cannot parse the response"
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
        env.env_message xmpp kind jid_from resp
    in
    let url = "http://export.yandex.ru/weather/?city=" ^ text in
      Http_suck.http_get url callback
  
let plugin opts =
  add_for_token
    (fun _opts xmpp ->
       add_commands xmpp [("blogs", blogs);
                          ("probki", traffic);
                          ("yaweather", weather)] opts
    )

let () =
  Plugin.add_plugin "yandex" plugin
  
