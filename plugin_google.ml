(*
 * (c) 2004-2010 Anastasia Gornostaeva
 *)

open Hooks
open Plugin_command
open Http_suck

let get xmpp env kind jid_from f url =
  let callback data =
    match data with
      | OK (_media, _charset, content) -> (
          try
            f content
          with _exn ->
            env.env_message xmpp kind jid_from
              (Lang.get_msg env.env_lang "plugin_google_bad_response" [])
        )
      | Exception exn ->
          let resp =
            match exn with
              | ClientError ->
                  Lang.get_msg env.env_lang "plugin_google_server_404" []
              | ServerError ->
                  Lang.get_msg env.env_lang "plugin_google_server_error" []
              | _ ->
                  Lang.get_msg env.env_lang "plugin_google_server_error" []
          in
            env.env_message xmpp kind jid_from resp
  in
    Http_suck.http_get url callback

open Json_type

type json google = < responseData : responseData;
                     responseDetails : string option;
                     responseStatus : int >
and responseData = < results : result array; cursor : cursor >
and result = < gSearchResultClass "GsearchResultClass" : string;
  unescapedUrl : string;
  url : string;
  visibleUrl : string;
  cacheUrl : string;
  title : string;
  titleNoFormatting : string;
  content : string >
and cursor = < ?pages : page array option;
               ?estimatedResultCount : string option;
               ?currentPageIndex : int option;
               moreResultsUrl : string >
and page = < start : string; label : int >

let google_search xmpp env kind jid_from text =
  if text = "" then
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_google_invalid_syntax" [])
  else
    let url = 
      "http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=" ^
        (Netencoding.Url.encode text)
    in
    let display data =
      let response = data#responseData in
      let msg, tail =
        if Array.length response#results > 0 then
          let r = (response#results).(0) in
            (Printf.sprintf "%s\n%s"
               (Xml.decode (Dehtml.html2text r#titleNoFormatting))
               (Xml.decode (Dehtml.html2text r#content)),
             r#unescapedUrl);
        else
          Lang.get_msg env.env_lang "plugin_google_no_answer" [], ""
      in
        match response#cursor#estimatedResultCount with
          | None -> (msg, tail)
          | Some v -> (msg, tail ^ "\nEstimated results " ^ v)
    in
    let callback content =
      let parsed = google_of_json (Json_io.json_of_string content) in
      let msg, response_tail = display parsed in
        env.env_message xmpp kind jid_from ~response_tail msg
    in
      get xmpp env kind jid_from callback url

type json google_translate =
    < rData "responseData" : rData option;
      responseDetails : string option;
      responseStatus : int >
and rData = < translatedText : string >

let cmd = Pcre.regexp ~flags:[`DOTALL; `UTF8]
  "^([a-zA-Z-]{2,})\\s+([a-zA-Z-]{2,})\\s(.+)"

let google_translate xmpp env kind jid_from text =
  if text = "" then
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_google_invalid_syntax" [])
  else
    let data =
      try
        let res = Pcre.exec ~rex:cmd text in
        let lg1 = Pcre.get_substring res 1 in
        let lg2 = Pcre.get_substring res 2 in
        let str = Pcre.get_substring res 3 in
          Some (lg1, lg2, str)
      with Not_found -> None
    in
      match data with
        | Some (lg1, lg2, str) ->
            let url =
              Printf.sprintf
                "http://ajax.googleapis.com/ajax/services/language/translate?v=1.0&q=%s&langpair=%s"
                (Netencoding.Url.encode str)
                (Netencoding.Url.encode (lg1 ^ "|" ^ lg2))
            in
            let display data =
              match data#rData with
                | None -> (
                    match data#responseDetails with
                      | None -> ""
                      | Some v -> v
                  )
                | Some v ->
                    v#translatedText
            in
            let callback content =
              let parsed =
                google_translate_of_json (Json_io.json_of_string content) in
              let msg = display parsed in
                env.env_message xmpp kind jid_from msg
            in
              get xmpp env kind jid_from callback url
        | None ->
            env.env_message xmpp kind jid_from
              (Lang.get_msg env.env_lang "plugin_google_invalid_syntax" [])
        
let plugin opts =
  add_for_token
    (fun _opts xmpp ->
       add_commands xmpp [("google", google_search);
                          ("translate", google_translate)] opts
    )

let _ =
  Plugin.add_plugin "google" plugin
