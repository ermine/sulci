(*                                                                          *)
(* (c) 2004, Anastasia Gornostaeva. <ermine@ermine.pp.ru                    *)
(*                                                                          *)

open Xml
open Xmpp
open Http_client
open Types

(* 
   doGoogleSearch method
   "key"        (google account)
   "q"          (query text),
   "start"      (where to start returning in the results),
   "maxResults" (number of allowed results),
   "filter"     (filter out very similar results),
   "restrict"   (country or topic restrictions),
   "safeSearch" (pornography filter), 
   "lr"         (language restrict), 
   "ie"         (input encoding)
   "oe"         (output encoding). 
*)

let google_key = trim (Xml.get_cdata Config.conf ~path:["google"; "key"])

let make_query start maxResults query =
   let filter = "true" in
   Xmlelement
      ("SOAP-ENV:Envelope",
       ["xmlns:SOAP-ENV", "http://schemas.xmlsoap.org/soap/envelope/";
	"xmlns:xsi", "http://www.w3.org/1999/XMLSchema-instance";
	"xmlns:xsd", "http://www.w3.org/1999/XMLSchema"],
       [Xmlelement 
	   ("SOAP-ENV:Body", [], 
	    [Xmlelement ("ns1:doGoogleSearch",
			 ["xmlns:ns1", "urn:GoogleSearch";
			  "SOAP-ENV:encodingStyle",
			  "http://schemas.xmlsoap.org/soap/encoding/"],
			 [Xmlelement ("key", 
				      ["xsi:type", "xsd:string"], 
				      [Xmlcdata google_key]);
			  Xmlelement ("q", 
				      ["xsi:type", "xsd:string"],
				      [Xmlcdata query]);
			  Xmlelement ("start",
				      ["xsi:type", "xsd:int"], 
				      [Xmlcdata start]);
			  Xmlelement ("maxResults",
				      ["xsi:type", "xsd:int"],
				      [Xmlcdata maxResults]);
			  Xmlelement ("filter",
				      ["xsi:type", "xsd:boolean"],
				      [Xmlcdata filter]);
			  Xmlelement ("restrict",
				      ["xsi:type", "xsd:string"], []);
			  Xmlelement ("safeSearch", 
				      ["xsi:type", "xsd:boolean"],
				      [Xmlcdata "false"]);
			  Xmlelement ("lr",
				      ["xsi:type", "xsd:string"], []);
			  Xmlelement ("ie",
				      ["xsi:type", "xsd:string"],
				      [Xmlcdata "latin1"]);
			  Xmlelement ("oe", 
				      ["xsi:type", "xsd:string"],
				      [Xmlcdata "latin1"])
			 ])])])

let html_ent = Str.regexp "&amp;#\\([0-9]+\\);"
let html = Str.regexp "&lt;/?\\(b\\|i\\|p\\|br\\)&gt;"

let strip_html text =
   let r1 = Str.global_replace html "" text in
   let r2 = 
      Str.global_substitute html_ent
	 (function x ->
	     let p = Str.matched_group 1 r1 in
	     let newstr = String.create 1 in
		newstr.[0] <- Char.chr (int_of_string p);
		newstr) r1 in
   let r3 = Str.global_replace (Str.regexp "&amp;lt;") "&lt;" r2 in
   let r4 = Str.global_replace (Str.regexp "&amp;gt;") "&gt;" r3 in
   let r5 = Str.global_replace (Str.regexp "&amp;quot;") "&quot;" r4 in
   let r6 = Str.global_replace (Str.regexp "&amp;apos;") "&apos;" r5 in
   let r7 = Str.global_replace (Str.regexp "&amp;amp;") "&amp;" r6 in
      r7

let message result =
   let text item tag = strip_html (get_cdata item ~path:[tag]) in
   let rec cycle lst acc = 
      if lst = [] then acc
      else
	 let item = List.hd lst in
	 let chunked = match item with
	    | Xmlelement (_, _, _) ->
		 Printf.sprintf "%s\n%s\n%s\n%s - %s"
		     (text item "title")
		     (text item "summary")
		     (text item "snippet")
		     (get_cdata item ~path:["URL"])
		     (text item "cachedSize");
	    | _ -> ""
	 in
	    cycle (List.tl lst) (acc ^ chunked)
   in
      cycle (get_subels result) ""

   let xmldecl = "<?xml version='1.0' encoding='UTF-8' ?>\r\n"

let google_search start items request =
   let soap = make_query start items request in
   let query = element_to_string soap in
      try
	 let content = Http_client.request "api.google.com"
			  (Http_client.Post ("/search/beta2", 
					     (xmldecl ^ query)))
			  ["Accept-Encoding: identity";
			   "SOAPAction: urn:GoogleSearchAction";
			   "Content-Type: text/xml; charset=utf-8"] in
	 let parsed = Xmlstring.parse_string content in
	 let result = Xml.get_tag parsed ["SOAP-ENV:Body"; 
					  "ns1:doGoogleSearchResponse";
					  "return";
					  "resultElements"] 
	 in
	    message result
      with HttpClientError err  -> err
(*	 | _ -> "Error getting data" *)

let google_spell request =
   let soap = 
      Xmlelement 
	 ("SOAP-ENV:Envelope", 
	  ["xmlns:SOAP-ENV","http://schemas.xmlsoap.org/soap/envelope/";
	   "xmlns:xsi", "http://www.w3.org/1999/XMLSchema-instance";
	   "xmlns:xsd", "http://www.w3.org/1999/XMLSchema"],
	  [Xmlelement ("SOAP-ENV:Body", [],
		       [Xmlelement 
			   ("ns1:doSpellingSuggestion", 
			    ["xmlns:ns1", "urn:GoogleSearch";
			     "SOAP-ENV:encodingStyle", 
			     "http://schemas.xmlsoap.org/soap/encoding/"],
			    [Xmlelement ("key", ["xsi:type", "xsd:string"],
					 [Xmlcdata google_key]);
			     Xmlelement ("phrase", 
					 ["xsi:type", "xsd:string"],
					 [Xmlcdata request])
			    ])])]) in
   let query = element_to_string soap in
      try
	 let content = Http_client.request "api.google.com"
			  (Http_client.Post ("/search/beta2", 
					     (xmldecl ^ query)))
			  ["Content-Type: text/xml; charset=utf-8"] in
	 let parsed = Xmlstring.parse_string content in
	 let response = 
	    Xml.get_cdata parsed ~path:["SOAP-ENV:Body"; 
					"ns1:doSpellingSuggestionResponse";
					"return"] in
	    if response = "" then "[нет ответа]" else response
      with HttpClientError err  -> err
	 | _ -> "Error getting data"

let rex = Str.regexp "google +\\(.+\\)"
let rex_adv = Str.regexp "google_adv +\\([0-9]\\) +\\([0-9]+\\) +\\([^\n]+\\)"
let rex_gspell = Str.regexp "gspell +\\([^\n]+\\)"

let google xml out bot mynick lang =
   let body = try Xml.get_cdata xml ~path:["body"] with _ -> "" in
      if Str.string_match rex body 0 then 
	 let request = Str.matched_group 1 body in
	 let proc (xml, out) =
	    let response = 
	       let r = google_search "0" "1"  request in
		  if r = "" then "Не нашёл :(" else r 
	    in
	       out (make_msg xml response)
	 in
	    ignore (Thread.create proc (xml, out))

      else if Str.string_match rex_adv body 0 then
	 let start = Str.matched_group 1 body in
	 let items = 
	    let z = Str.matched_group 2 body in
	       if int_of_string z > 10 then "10" else z in
	 let request = Str.matched_group 3 body in
	 let proc (xml, out) =
	    let response = 
	       let r = google_search start items request in
		  if r = "" then "Не нашёл :(" else r
	    in
	       out (make_msg xml response)
	 in
	    ignore (Thread.create proc (xml, out))
      else if Str.string_match rex_gspell body 0 then
	 let request = Str.matched_group 1 body in
	 let proc (xml, out) =
	    let response = google_spell request in
	       out (make_msg xml response)
	 in
	    ignore (Thread.create proc (xml, out))

let _ =
   Muc.register_cmd "google" google;
   Muc.register_help "google" 
"google поисковое выражение
   Поиск в Google";
   Muc.register_cmd "gspell" google;
   Muc.register_help "gspell"
"gspell проверяемое выражение
   Проверка орфорграфии с помощью Google Spelling";

