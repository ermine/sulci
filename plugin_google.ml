(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xml
open Xmpp
open Http_client
open Common

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

let google_key = trim (Xml.get_cdata Config.config 
			  ~path:["plugins"; "google"; "key"])

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

let html_ent = Pcre.regexp "&amp;#([0-9]+);"
let html = Pcre.regexp "&lt;/?(b|i|p|br)&gt;"
let amp = Pcre.regexp "&amp;(lt|gt|quot|apos|amp);"

let strip_html text =
   let r1 = Pcre.qreplace ~rex:html ~templ:"" text in
   let r2 = 
      Pcre.substitute_substrings ~rex:html_ent
	 ~subst:(fun x ->
		    let p = Pcre.get_substring x 1 in
		    let newstr = String.create 1 in
		       newstr.[0] <- Char.chr (int_of_string p);
		       newstr) r1 in
   let r3 = Pcre.substitute_substrings ~rex:amp
	       ~subst:(fun x -> "&" ^ (Pcre.get_substring x 1) ^ ";") r2
   in r3

let message result =
   let text item tag = strip_html (get_cdata item ~path:[tag]) in
   let rec cycle lst acc = 
      if lst = [] then acc
      else
	 let item = List.hd lst in
	 let chunked = match item with
	    | Xmlelement (_, _, _) ->
		 Printf.sprintf "%s%s%s%s - %s"
		    (let t = text item "title" in
			if t = "" then "" else t ^ "\n")
		    (let t = text item "summary" in
			if t = "" then "" else t ^ "\n")
		    (let t = text item "snippet" in
			if t = "" then "" else t ^ "\n")
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

let google text xml out =
   if text = "" then
      out (make_msg xml 
	      (Lang.get_msg ~xml "plugin_google_invalid_syntax" []))
   else
      let proc () =
	 let response = 
	    let r = google_search "1" "1" text in
	       if r = "" then 
		  Lang.get_msg ~xml "plugin_google_not_found" []
	       else r 
	 in
	    out (make_msg xml response)
      in
	 ignore (Thread.create proc ())

let rx = Pcre.regexp "([0-9]+) ([1-9]{1}) (.+)"
let google_adv text xml out =
   try
      let r = Pcre.exec ~rex:rx text in
      let start = Pcre.get_substring r 1 in
      let limit = Pcre.get_substring r 2 in
      let request = Pcre.get_substring r 3 in
      let proc () =
	 let response = 
	    let r = google_search start limit request in
	       if r = "" then 
		  Lang.get_msg ~xml "plugin_google_not_found" []
	       else r
	 in
	    out (make_msg xml response)
      in
	 ignore (Thread.create proc ())
   with Not_found ->
      out (make_msg xml 
	      (Lang.get_msg ~xml "plugin_google_adv_invalid_syntax" []))

let gspell text xml out =
   if text = "" then
      out (make_msg xml 
	      (Lang.get_msg ~xml "plugin_google_invalid_suntax" []))
   else
      let proc () =
	 let response = google_spell text in
	    out (make_msg xml response)
      in
	 ignore (Thread.create proc ())

let _ =
   Hooks.register_handle (Hooks.Command ("google", google));
   Hooks.register_handle (Hooks.Command ("google_adv", google_adv));
   Hooks.register_handle (Hooks.Command ("gspell", gspell))
