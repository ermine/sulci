(*                                                                          *)
(* (c) 2004, 2005, 2006 Anastasia Gornostaeva. <ermine@ermine.pp.ru>        *)
(*                                                                          *)

open Xml
open Xmpp
open Common
open Http_suck

let blogs text event from xml out =
   if text = "" then
      make_msg out xml 
	 (Lang.get_msg ~xml "plugin_yandex_bad_syntax" [])
   else
      let callback data =
	 let resp = match data with
	    | OK content ->
		 let parsed = Xmlstring_netstring.parse_string content in
		    (try
			let item = Xml.get_tag parsed ["channel"; "item"] in
			let title = Dehtml.html2text 
			   (Xml.get_cdata item ~path:["title"]) in
			let pubdate = Xml.get_cdata item ~path:["pubDate"] in
			let link = Xml.get_cdata item ~path:["link"] in
			let descr = Dehtml.html2text 
			   (Xml.get_cdata item ~path:["description"]) in
			   Printf.sprintf "%s\n%s\n%s\n%s"
			      (trim title) (trim descr) link pubdate
		     with Not_found ->
			Lang.get_msg ~xml "plugin_yandex_not_found" [])
	    | Exception exn ->
		 match exn with
		    | ClientError ->
			 Lang.get_msg ~xml "plugin_yandex_404" []
		    | ServerError ->
			 Lang.get_msg ~xml "plugin_yandex_server_error" []
		    | _ ->
			 Lang.get_msg ~xml "plugin_yandex_server_error" []
	 in
	    make_msg out xml resp
      in
      let url = 
  "http://blogs.yandex.ru/search.rss?how=tm&rd=2&charset=UTF-8&numdoc=1&text=" ^
	    Netencoding.Url.encode (Xml.decode text) in
	 Http_suck.http_get url callback

let _ =
   Hooks.register_handle (Hooks.Command ("blogs", blogs));