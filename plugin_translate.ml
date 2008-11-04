(*
 * (c) 2005-2008 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

(* wap.translate.ru/default.asp?cp=cyr&dir=er&source=hello *)

open Pcre
open Nethtml
open Xml
open Printf
open Common
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
    "http://wap.translate.ru/wap2/translator.aspx/result/?tbDirection=%s&tbText=%s&submit=%s"
    lang (Netencoding.Url.encode (Xml.decode text))
    (Netencoding.Url.encode "Перевести")

(* translate er text *)
let cmd = Pcre.regexp ~flags:[`DOTALL; `UTF8] "^([a-zA-Z]{2,2})\\s+(.+)"

let process_doc doc =
  let get_data els =
    let rec aux_get_data = function
      | [] -> ""
      | x :: xs ->
          match x with
            | Data data -> data
            | _ -> aux_get_data xs
    in
      aux_get_data els
  in
  let rec aux_find (acc: string list option) = function
    | [] -> acc
    | x :: xs ->
        match x with
          | Element (tag, attrs, els) ->
              if tag = "p" &&
                (try List.assoc "class" attrs with Not_found -> "") = "result" then
                  let newacc = match acc with
                    | None -> Some [get_data els]
                    | Some v -> Some ((get_data els) :: v)
                  in
                    aux_find newacc xs
              else (
                if acc = None then
                  match aux_find acc els with
                    | None -> aux_find acc xs
                    | Some v -> Some v
                else
                  aux_find acc xs
              )
          | _ ->
              aux_find acc xs
  in
    aux_find None doc

let do_request lang text xml out =
  let callback data =
    let resp = match data with
	    | OK (_media, _charset, content) -> (
          try
            let ch = new Netchannels.input_string content in
            let doc = parse ~dtd:relaxed_html40_dtd ~return_declarations:false
                ~return_pis:false ~return_comments:false ch in
            let res = process_doc doc in
              match res with
                | None -> Lang.get_msg ~xml "plugin_translate_not_parsed" []
                | Some vl ->
                    trim (List.hd vl)
          with exc ->
            Lang.get_msg ~xml "plugin_translate_not_parsed" []
        )
	    | Exception exn ->
	        Lang.get_msg ~xml "plugin_translate_server_error" []
    in
	    make_msg out xml resp
  in
    Http_suck.http_get (url lang text) callback

let translate text event from xml out =
  match trim(text) with
    | "list" ->
	      do_list xml out
    | other ->
        try
		      let res = exec ~rex:cmd ~pos:0 text in
		      let lang = String.lowercase (get_substring res 1)
		      and str = get_substring res 2 in
            if List.mem_assoc lang langpairs then
			        do_request lang str xml out
		        else
			        make_msg out xml 
			          (Lang.get_msg ~xml "plugin_translate_bad_language"
				          [])
	      with Not_found ->
		      make_msg out xml 
		        (Lang.get_msg ~xml "plugin_seen_bad_syntax" [])
		          
let _ =
  Hooks.register_handle (Hooks.Command ("tr", translate))
