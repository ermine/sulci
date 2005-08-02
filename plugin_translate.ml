(*                                                                          *)
(* (c) 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>                    *)
(*                                                                          *)

(* wap.translate.ru/default.asp?cp=cyr&dir=er&source=hello *)

open Midge
open Pcre
open Xml
open Printf
open Common

let _ =
   let translate =
      let url lang text =
	 Printf.sprintf 
	    "http://wap.translate.ru/default.asp?cp=cyr&dir=%s&source=%s"
	    lang (Netencoding.Url.encode text)
      in
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
      ] in
      let langmap = [
	 "en", ["ru"; "ge"; "sp"; "fr"; "pt"];
	 "ru", ["en"; "ge"; "fr"; "sp"];
	 "fr", ["en"; "ru"; "sp"];
	 "ge", ["ru"; "en"];
	 "sp", ["ru"; "en"];
	 "it", ["ru"]
      ] in
      let do_list =
	 let list = "\n" ^ String.concat "\n" 
	    (List.map (fun (a,b) -> a ^ "\t" ^ b) languages)
	 in
	    fun xml out -> out (make_msg xml list)
      in
      let do_translate (lg, str) =
	 let content = Midge.simple_get (url lg str) in
	 let wml = Xmlstring.parse_string content in
	 let p = Xml.get_tag wml ["card";"p"] in
	    match p with
	       | Xmlelement (_, _, els) ->
		    element_to_string (List.nth els 2)
	       | _ -> 
		    "bad result"
      in
      let do_translate_via (lg1, lg2, str) =
	 let content1 = do_translate (lg1, str) in
	 let content2 = do_translate (lg2, content1) in
	    content2
      in
	 (* translate er text *)
      let rex1 = Pcre.regexp ~flags:[`DOTALL; `UTF8] 
	 "^([a-zA-Z]{2,2})\\s+(.+)" in
      let rex2 = Pcre.regexp ~flags:[`DOTALL; `UTF8]
	 "^-(from|to)\\s+([a-zA-Z]{2,2})\\s+-(from|to)\\s+([a-zA-A]{2,2})\\s+(-via\\s+([a-zA-Z]{2,2})\\s+)?(.+)" in

	 fun text event from xml out ->
	    if trim(text) = "list" then
	       do_list xml out
	    else
	       try
		  let res = exec ~rex:rex1 ~pos:0 text in
		  let lg = String.lowercase (get_substring res 1)
		  and str = get_substring res 2 in
		     if List.mem_assoc lg languages then
			let proc (lang, request, xml, out) =
			   out (make_msg xml (do_translate(lang, request)))
			in
			   ignore (Thread.create proc (lg, str, xml, out))
	       with Not_found ->
		 print_endline "try second regexp";
		  flush Pervasives.stdout;
		  try
		     print_endline "matched";
		     flush Pervasives.stdout;
		     let res = exec ~rex:rex2 ~pos:0 text in
		     let par1 = get_substring res 1 in
		     let lg1 = get_substring res 2 in
		     let par2 = get_substring res 3 in
		     let lg2 = get_substring res 4 in
		     let via = try 
			Some (get_substring res 6) with Not_found -> None in
		     let str = get_substring res 7 in
			if par1 = par2 && via = None then
			   out (make_msg xml "Так неинтересно")
			else
			   match via with
			      | None ->
				   let lgfrom, lgto = if par1 = "from" then
				      lg1, lg2 else lg2, lg1 in
				      if List.mem_assoc lgfrom langmap &&
					 List.mem lgto 
					 (List.assoc lgfrom langmap) then
					    let lg = String.make 1 lgfrom.[0] ^ 
					       String.make 1 lgto.[0] in
			let proc (lang, request, xml, out) =
			   out (make_msg xml (do_translate(lang, request)))
			in
			   ignore (Thread.create proc (lg, str, xml, out))
				      else
					 out (make_msg xml "Плохой вариант")
			      | Some vl ->
				   let lgfrom, lgto = if par1 = "from" then
				      lg1, lg2 else lg2, lg1 in
				      if List.mem_assoc lgfrom langmap &&
					 List.mem vl
					 (List.assoc lgfrom langmap) &&
					 List.mem_assoc vl langmap &&
					 List.mem lgto (List.assoc vl langmap)
				      then
					 let lang1 = String.make 1 lg1.[0] ^
					    String.make 1 vl.[0]
					 and lang2 = String.make 1 vl.[0] ^
					    String.make 1 lg2.[0] in
			let proc (lg1, lg2, request, xml, out) =
			   out (make_msg xml (do_translate_via (lg1, lg2, request)))
			in
			   ignore (Thread.create proc (lang1, lang2, str, xml, out))
		  with Not_found ->
		     print_endline "nowhere matched";
		     flush Pervasives.stdout
   in
      Hooks.register_handle (Hooks.Command ("tr", translate))
