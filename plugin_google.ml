(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Hooks
open Plugin_command
open Http_suck

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

let display env data =
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

let google_search xmpp env kind jid_from text =
  if text = "" then
    env.env_message xmpp kind jid_from (Lang.get_msg env.env_lang
                                          "plugin_google_invalid_syntax" [])
  else
    let callback data =
      let resp =
        match data with
          | OK (_media, _charset, content) -> (
              try
                let parsed = google_of_json (Json_io.json_of_string content) in
                  display env parsed
              with _exn ->
                Lang.get_msg env.env_lang "plugin_google_bad_response" [], ""
            )
          | Exception exn ->
              match exn with
                | ClientError ->
                    Lang.get_msg env.env_lang "plugin_google_server_404" [], ""
                | ServerError -> 
                    Lang.get_msg env.env_lang "plugin_google_server_error" [], ""
                | _ ->
                    Lang.get_msg env.env_lang "plugin_google_server_error" [], ""
      in
      let msg, response_tail = resp in
        env.env_message xmpp kind jid_from ~response_tail msg
    in
    let url = 
      "http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=" ^
        (Netencoding.Url.encode text)
    in
      Http_suck.http_get url callback

let plugin opts =
  add_commands [("google", google_search)] opts

let _ =
  add_plugin "google" plugin
