(*
 * (c) 2005-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

(*
  prev: wap.translate.ru/default.asp?cp=cyr&dir=er&source=hello
  curr: m.translate.ru/translator/result/?dirCode=er&text=hello
*)

open Pcre
open Xml
open Types
open Common
open Hooks
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

let do_list =
  let list = "\n" ^ String.concat "\n"
    ((List.map (fun (a, l) -> a ^ "\t" ^ l) langpairs))
  in
    fun xml out -> make_msg out xml list
      
let url lang text =
  Printf.sprintf
    "http://m.translate.ru/translator/result/?dirCode=%s&text=%s"
    lang (Netencoding.Url.encode (Xml.decode text))
    
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
          | _ ->
              aux_find xs
  in
    aux_find wml
      
let do_request language text xml env out =
  let callback data =
    let resp = match data with
      | OK (_media, _charset, content) -> (
          try
            let wml = Xmlstring.parse_string content in
              match process_doc (get_subels wml) with
                | None -> Lang.get_msg env.env_lang "plugin_translate_not_parsed" []
                | Some r -> r
          with exc ->
            Lang.get_msg env.env_lang "plugin_translate_not_parsed" []
        )
      | Exception exn ->
          Lang.get_msg env.env_lang "plugin_translate_server_error" []
    in
      make_msg out xml resp
  in
    Http_suck.http_get (url language text) callback
      
let translate text from xml env out =
  match trim(text) with
    | "list" ->
        do_list xml out
    | other ->
        try
          let res = exec ~rex:cmd ~pos:0 text in
          let language = String.lowercase (get_substring res 1)
          and str = get_substring res 2 in
            if List.mem_assoc language langpairs then
              do_request language str xml env out
            else
              make_msg out xml 
                (Lang.get_msg env.env_lang "plugin_translate_bad_language" [])
        with Not_found ->
          make_msg out xml 
            (Lang.get_msg env.env_lang "plugin_translate_bad_syntax" [])
            
let _ =
  register_command "tr" translate
