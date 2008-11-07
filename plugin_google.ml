(*
 * (c) 2004-2008 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open Xmpp
open Common
open Http_suck

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

let google_key = 
  try trim (Xml.get_cdata Config.config 
		~path:["plugins"; "google"; "key"]) with Not_found ->
    Printf.eprintf "Cannot find an google key in config file\n";
    Pervasives.exit 127
      
let make_query start maxResults query =
  let filter = "false" in
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
				["xsi:type", "xsd:string"], 
				[]);
			  Xmlelement ("safeSearch", 
				["xsi:type", "xsd:boolean"],
				[Xmlcdata "false"]);
			  Xmlelement ("lr",
				["xsi:type", "xsd:string"], 
				[]);
			  (* [Xmlcdata "lang_ru"]; *)
			  Xmlelement ("ie",
				["xsi:type", "xsd:string"],
				[]);
			  Xmlelement ("oe", 
				["xsi:type", "xsd:string"],
				[])
			  ])])])
      
(*
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
*)

let strip_html = Dehtml.html2text

let message items =
  let count = ref 0 in
  let text item tag = strip_html (get_cdata item ~path:[tag]) in
  let rec cycle lst acc = 
    if lst = [] then acc
    else
	    let item = List.hd lst in
	    let chunked = match item with
	      | Xmlelement (_, _, _) ->
		        incr count;
		        Printf.sprintf "%d: %s%s%s%s%s\n"
		          !count
		          (let t = text item "title" in
			          if t = "" then "" else t ^ "\n")
		          (let t = text item "summary" in
			          if t = "" then "" else t ^ "\n")
		          (let t = text item "snippet" in
			          if t = "" then "" else t ^ "\n")
		          (get_cdata item ~path:["URL"])
		          (let t = text item "cachedSize" in 
			          if t = "" then "" else " - " ^ t)
	      | _ -> ""
	    in
	      cycle (List.tl lst) (acc ^ chunked)
  in
    cycle items ""
      
let one_message item =
  let text tag = strip_html (get_cdata item ~path:[tag]) in
    Printf.sprintf "%s%s%s"
	    (let t = text "title" in if t = "" then "" else t ^ "\n")
	    (let t = text "summary" in if t = "" then "" else t ^ "\n")
	    (let t = text "snippet" in if t = "" then "" else t),
  (get_cdata item ~path:["URL"]) ^ 
    (let t = text "cachedSize" in if t = "" then "" else " - " ^ t)
    
let xmldecl = "<?xml version='1.0' encoding='UTF-8' ?>\r\n"
  
let gspell text event from xml out =
  if text = "" then
    make_msg out xml (Lang.get_msg ~xml "plugin_google_invalid_syntax" [])
  else
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
					[Xmlcdata text])
			    ])])]) in
    let query = element_to_string soap in
    let callback data =
	    let resp = match data with
	      | OK (_media, _charset, content) ->
		        let parsed = Xmlstring.parse_string content in
		        let response = 
		          Xml.get_cdata parsed 
		            ~path:["SOAP-ENV:Body"; 
			          "ns1:doSpellingSuggestionResponse";
			          "return"] in
		          if response = "" then 
		            Lang.get_msg ~xml "plugin_google_no_answer" []
		          else response
	      | Exception exn ->
		        match exn with
		          | ClientError ->
			            Lang.get_msg ~xml "plugin_google_server_404" []
		          | ServerError -> 
			            Lang.get_msg ~xml "plugin_google_server_error" []
		          | _ ->
			            Lang.get_msg ~xml "plugin_google_server_error" []
	    in
	      make_msg out xml resp
    in
	    Http_suck.http_post "http://api.google.com/search/beta2"
	      ["Content-Type", "text/xml; charset=utf-8"] 
	      (xmldecl ^ query) callback
	      
let google ?(start="0") ?(items="1") text event from xml out =
  if text = "" then
    make_msg out xml (Lang.get_msg ~xml "plugin_google_invalid_syntax" [])
  else
    let callback data =
	    let resp, tail = match data with
	      | OK (_media, _charset, content) ->
		        let parsed = Xmlstring.parse_string content in
		        let result = Xml.get_tag parsed ["SOAP-ENV:Body"; 
						"ns1:doGoogleSearchResponse";
						"return";
						"resultElements"] in
		          if items = "1" then
		            let item = get_tag result ["item"] in
		            let r1, r2 = one_message item in
			            if r1 = "" && r2 = "" then
			              (Lang.get_msg ~xml "plugin_google_not_found" [], 
			              "")
			            else
			              r1, r2
		          else
		            let r = message (get_subels result) in
			            if r = "" then
			              (Lang.get_msg ~xml "plugin_google_not_found" [],
			              "")
			            else r, ""
	      | Exception exn ->
		        match exn with
		          | ClientError ->
			            Lang.get_msg ~xml "plugin_google_server_404" [], ""
		          | ServerError ->
			            Lang.get_msg ~xml "plugin_google_server_error" [], ""
		          | _ ->
			            Lang.get_msg ~xml "plugin_google_server_error" [], ""
	    in
	    let response_tail = if tail = "" then None else Some tail in
	      make_msg out xml ?response_tail resp 
    in
    let soap = make_query start items text in
	    Http_suck.http_post "http://api.google.com/search/beta2"
	      ["Accept-Encoding", "identity";
	      "SOAPAction", "urn:GoogleSearchAction";
	      "Content-Type", "text/xml; charset=utf-8"]
	      (xmldecl ^ element_to_string soap)
	      callback
        
let rx = Pcre.regexp "([0-9]+) ([1-9]{1}) (.+)"
  
let google_adv text event from xml out =
  try
    let r = Pcre.exec ~rex:rx text in
    let start = Pcre.get_substring r 1 in
    let items = Pcre.get_substring r 2 in
    let request = Pcre.get_substring r 3 in
	    google ~start ~items request event from xml out
  with Not_found ->
    make_msg out xml 
	    (Lang.get_msg ~xml "plugin_google_adv_invalid_syntax" [])
      
let _ =
  Hooks.register_handle (Hooks.Command ("google", google));
  Hooks.register_handle (Hooks.Command ("google_adv", google_adv));
  Hooks.register_handle (Hooks.Command ("gspell", gspell))
