(*
 * (c) 2005-2012 Anastasia Gornostaeva
 *)

(*
  prev: wap.translate.ru/default.asp?cp=cyr&dir=er&source=hello
  curr: m.translate.ru/translator/result/?dirCode=er&text=hello
*)

open Pcre
open Light_xml
open Common
open Hooks
open Plugin_command
open Http_suck

let langpairs = [
  "er", "English-Russian";
  "re", "Russian-English";
  "gr", "German-Russian";
  "rg", "Russian-German";
  "fr", "French-Russian";
  "rf", "Russian-French";
  "ir", "Italian-Russian";
  "sr", "Spanish-Russian";
  "rs", "Russian-Spanish";
  "eg", "English-German";
  "ge", "German-English";
  "ef", "English-French";
  "fe", "French-English";
  "fg", "French-German";
  "gf", "German-French";
  "es", "English-Spanish";
  "se", "Spanish-English";
  "fs", "French-Spanish";
  "sf", "Spanish-French";
  "ep", "English-Portuguese";
  "pe", "Portuguese-English";
  "gs", "German-Spanish";
  "sg", "Spanish-German";
  "ie", "Italian-Englsish"
]

let do_list xmpp env kind jid_from =
  let list = "\n" ^ String.concat "\n"
    ((List.map (fun (a, l) -> a ^ "\t" ^ l) langpairs))
  in
    env.env_message xmpp kind jid_from list
      
let url lang text =
  Printf.sprintf
    "http://m.translate.ru/translator/result/?dirCode=%s&text=%s"
    lang (Netencoding.Url.encode (decode text))
    
(* command: tr er text *)
let cmd = Pcre.regexp ~flags:[`DOTALL; `UTF8] "^([a-zA-Z]{2,2})\\s+(.+)"
  
let process_doc wml =
  let rec aux_find = function
    | [] -> None
    | x :: xs ->
        match x with
          | Xmlelement (tag, attrs, els) ->
              if tag = "div" &&
                (try List.assoc "class" attrs with Not_found -> "") = "tres" then
                  Some (get_cdata x)
              else (
                match aux_find els with
                  | None -> aux_find xs
                  | Some r -> Some r
              )
          | Xmlcdata _ ->
              aux_find xs
  in
    aux_find wml
      
let do_request language xmpp env kind jid_from text =
  let callback data =
    let resp = match data with
      | OK (_media, _charset, content) -> (
          try
            let wml = Light_xml.parse_string content in
              match process_doc (get_subels wml) with
                | None -> Lang.get_msg env.env_lang
                    "plugin_translate_not_parsed" []
                | Some r -> r
          with _exc ->
            Lang.get_msg env.env_lang "plugin_translate_not_parsed" []
        )
      | Exception _exn ->
          Lang.get_msg env.env_lang "plugin_translate_server_error" []
    in
      env.env_message xmpp kind jid_from resp
  in
    Http_suck.http_get (url language text) callback
      
let translate xmpp env kind jid_from text =
  match trim(text) with
    | "list" ->
        do_list xmpp env kind jid_from
    | _ ->
        try
          let res = exec ~rex:cmd ~pos:0 text in
          let language = String.lowercase (get_substring res 1)
          and str = get_substring res 2 in
            if List.mem_assoc language langpairs then
              do_request language xmpp env kind jid_from str
            else
              env.env_message xmpp kind jid_from 
                (Lang.get_msg env.env_lang "plugin_translate_bad_language" [])
        with Not_found ->
          env.env_message xmpp kind jid_from
            (Lang.get_msg env.env_lang "plugin_translate_bad_syntax" [])
            
let plugin opts =
  add_for_token
    (fun _opts xmpp ->
       add_commands xmpp [("tr", translate)] opts
    )

let _ =
  Plugin.add_plugin "translate" plugin
