(*                                                                          *)
(* (c) 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>                    *)
(*                                                                          *)

(* wap.translate.ru/default.asp?cp=cyr&dir=er&source=hello *)

open Pcre
open Xml
open Printf
open Common
open Http_suck

let languages = [
   "er", "English-Russian";
   "re", "Russian-English";
   "gr", "German-Russian";
   "rg", "Russian-German";
   "fr", "French-Russian";
   "rf", "Russian-French";
   "sr", "Spanish-Russian";
   "rs", "Russian-Spanish";
   "ir", "Italian-Russian";
   "eg", "English-German";
   "ge", "German-English";
   "es", "English-Spanish";
   "se", "Spanish-English";
   "ef", "English-French";
   "fe", "French-English";
   "ep", "English-Portuguese"
]

let langmap = [
   "en", ["ru"; "ge"; "sp"; "fr"; "pt"];
   "ru", ["en"; "ge"; "fr"; "sp"];
   "fr", ["en"; "ru"; "sp"];
   "ge", ["ru"; "en"];
   "sp", ["ru"; "en"];
   "it", ["ru"]
]

let do_list =
   let list = "\n" ^ String.concat "\n" 
      (List.map (fun (a,b) -> a ^ "\t" ^ b) languages)
   in
      fun xml out -> out (make_msg xml list)

let  do_map =
   let list = "\n" ^ String.concat "\n"
      ((List.map (fun (a, l) -> a ^ " [" ^ String.concat ", " l ^ "]") langmap))
   in
      fun xml out -> out (make_msg xml list)

let url lang text =
   let text =
      let lexbuf = Ulexing.from_utf8_string text in
	 Xmlstring.decode_xml "" lexbuf
   in
   Printf.sprintf 
      "http://wap.translate.ru/default.asp?cp=cyr&dir=%s&source=%s"
      lang (Netencoding.Url.encode text)

(* translate er text *)
let rex1 = Pcre.regexp ~flags:[`DOTALL; `UTF8] "^([a-zA-Z]{2,2})\\s+(.+)"
let rex2 = Pcre.regexp ~flags:[`DOTALL; `UTF8] 
   "^([a-zA-Z]{2,2})\\s+([a-z]{2,2})\\s+(.+)"
(*
let rex2 = Pcre.regexp ~flags:[`DOTALL; `UTF8]
   "^-(from|to)\\s+([a-zA-Z]{2,2})\\s+-(from|to)\\s+([a-zA-Z]{2,2})\\s+(-via\\s+([a-zA-Z]{2,2})\\s+)?(.+)" in
*)

let do_request lang text xml out =
   let callback data =
      let resp = match data with
	 | OK content ->
              let wml = Xmlstring.parse_string content in
              let p = Xml.get_tag wml ["card";"p"] in
		 (match p with
		     | Xmlelement (_, _, els) ->
			  element_to_string (List.nth els 2)
		     | _ -> 
			  "bad result")
	 | Exception exn ->
	      "some problems"
      in
	 out (make_msg xml resp)
   in
      Http_suck.http_get (url lang text) callback

let translate text event from xml out =
   match trim(text) with
      | "list" ->
	   do_list xml out
      | "map" ->
	   do_map xml out
      | other ->
	   try
	      let res = exec ~rex:rex2 text in
	      let lg1 = get_substring res 1 in
		 let lg2 = get_substring res 2 in
		 let str = get_substring res 3 in
		 let a = List.assoc lg1 langmap in
		    if List.mem lg2 a then
		       let lang = String.make 2 lg1.[0] in
			  lang.[1] <- lg2.[0];
			  do_request lang str xml out
	   with Not_found ->
              try
		 let res = exec ~rex:rex1 ~pos:0 text in
		    let lang = String.lowercase (get_substring res 1)
		    and str = get_substring res 2 in
                       if List.mem_assoc lang languages then
			  do_request lang str xml out
	      with Not_found ->
		 out (make_msg xml "not found")
		    
let _ =
   Hooks.register_handle (Hooks.Command ("tr", translate))
