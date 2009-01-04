(*
 * (c) 2008-2009 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Nethtml
open Netconversion
open Pcre
open Xml
open Types
open Common
open Hooks
open Http_suck

let slist = [
  "auto", "Detect language";
  "ar", "Arabic";
  "bg", "Bulgarian";
  "ca", "Catalan";
  "zh-CN", "Chinese";
  "hr", "Croatian";
  "cs", "Czech";
  "da", "Danish";
  "nl", "Dutch";
  "en", "English";
  "tl", "Filipino";
  "fi", "Finnish";
  "fr", "French";
  "de", "German";
  "el", "Greek";
  "iw", "Hebrew";
  "hi", "Hindi";
  "id", "Indonesian";
  "it", "Italian";
  "ja", "Japanese";
  "ko", "Korean";
  "lv", "Latvian";
  "lt", "Lithuanian";
  "no", "Norwegian";
  "pl", "Polish";
  "pt", "Portuguese";
  "ro", "Romanian";
  "ru", "Russian";
  "sr", "Serbian";
  "sk", "Slovak";
  "sl", "Slovenian";
  "es", "Spanish";
  "sv", "Swedish";
  "uk", "Ukrainian";
  "vi", "Vietnamese";
]
  
let tlist = [
  "ar", "Arabic";
  "bg", "Bulgarian";
  "ca", "Catalan";
  "zh-CN", "Chinese (Simplified)";
  "zh-TW", "Chinese (Traditional)";
  "hr", "Croatian";
  "cs", "Czech";
  "da", "Danish";
  "nl", "Dutch";
  "en", "English";
  "tl", "Filipino";
  "fi", "Finnish";
  "fr", "French";
  "de", "German";
  "el", "Greek";
  "iw", "Hebrew";
  "hi", "Hindi";
  "id", "Indonesian";
  "it", "Italian";
  "ja", "Japanese";
  "ko", "Korean";
  "lv", "Latvian";
  "lt", "Lithuanian";
  "no", "Norwegian";
  "pl", "Polish";
  "pt", "Portuguese";
  "ro", "Romanian";
  "ru", "Russian";
  "sr", "Serbian";
  "sk", "Slovak";
  "sl", "Slovenian";
  "es", "Spanish";
  "sv", "Swedish";
  "uk", "Ukrainian";
  "vi", "Vietnamese";
]

let list_languages =
  "\n" ^ String.concat "\n" (List.map (fun (a, b) -> a ^ "\t" ^ b) slist)
  
let process_result doc =
  let get_data els =
    let rec aux_get_data acc = function
      | [] -> acc
      | x :: xs ->
          match x with
            | Data data -> aux_get_data (acc ^ data) xs
            | Element (tag, _, _) ->
                if tag = "br" then
                  aux_get_data (acc ^ "\n") xs
                else
                  aux_get_data acc xs
    in
      aux_get_data "" els
  in
  let rec aux_find = function
    | [] -> None
    | x :: xs ->
        match x with
          | Element (tag, attrs, els1) ->
              if tag = "div" &&
                (try List.assoc "id" attrs with Not_found -> "") =
                "result_box" then
                  Some (get_data els1)
              else (
                match aux_find els1 with
                  | None -> aux_find xs
                  | Some v -> Some v
              )
          | _ ->
              aux_find xs
  in
    aux_find doc
      
let translate_text sl tl text xml env out =
  let callback data =
    let resp = match data with
      | OK (media, charset, content) -> (
          try
            let enc = 
              match charset with
                | None -> `Enc_iso88591
                | Some v ->
                    encoding_of_string v
            in
            let data =
              if enc <> `Enc_utf8 then
                convert ~in_enc:enc ~out_enc:`Enc_utf8 content
              else
                content
            in
            let ch = new Netchannels.input_string data in
            let doc = parse ~dtd:relaxed_html40_dtd ~return_declarations:false
                ~return_pis:false ~return_comments:false ch in
              match process_result doc with
                | None -> ""
                | Some v -> v
          with exn ->
            Lang.get_msg env.env_lang "plugin_google_translate_not_parsed" []
        )
      | Exception exn ->
          Lang.get_msg env.env_lang "plugin_google_translate_server_error" []
    in
      make_msg out xml resp
  in
  let url = "http://translate.google.com/translate_t" in
  let data = Printf.sprintf "langpair=%s|%s&ie=UTF8&oe=UTF8&text=%s"
    sl tl (Netencoding.Url.encode (Xml.decode text))
  in
    Http_suck.http_post url [] data callback
      
(* gtr er ru text *)
let cmd = Pcre.regexp ~flags:[`DOTALL; `UTF8]
  "^([a-zA-Z-]{2,5})\\s+([a-zA-Z-]{2,5})\\s(.+)"
  
let rm_newlines = Pcre.regexp "[\n\r]"
  
let translate text from xml env out =
  match trim(text) with
    | "list" ->
        make_msg out xml list_languages
    | other ->
        try
          let res = exec ~rex:cmd text in
          let lg1 = get_substring res 1 in
          let lg2 = get_substring res 2 in
          let str = get_substring res 3 in
            if List.mem_assoc lg1 slist then
              if List.mem_assoc lg2 tlist then
                translate_text lg1 lg2 str xml env out
              else
                raise Not_found
            else
              raise Not_found
        with Not_found ->
          make_msg out xml 
            (Lang.get_msg env.env_lang "plugin_google_translate_bad_syntax" [])
            
let _ = 
  register_command "gtr" translate
