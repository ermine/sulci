(*                                                                          *)
(* (c) 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>                    *)
(*                                                                          *)

open Netcgi
open Netcgi_types
open Netcgi_fcgi
open Printf
open Xml
open Unix

let port = 5221

let read_from_file file =
   let b = Buffer.create 100 in
   let f = open_in file in
      (try 
	  while true do
	     let line = input_line f in
		Buffer.add_string b line;
		Buffer.add_string b "\n";
	  done
       with End_of_file -> ());
      Buffer.contents b

let begin_page =
   let content = read_from_file "header.inc" in
      fun out ->
	 out content
	    
let end_page =
   let content = read_from_file "footer.inc" in
      fun out ->
	 out content

let field_list = ["user";
		  "fn";
		  "given";
		  "middle";
		  "family";
		  "nickname";
		  "bday";
		  "city";
		  "locality";
		  "email";
		  "orgname";
		  "orgunit"
		 ]

(* let text = Netencoding.Html.encode_from_latin1 *)
let text = Netencoding.Html.encode ~in_enc:`Enc_utf8 ~out_enc:`Enc_utf8 ()

let new_id =
   let counter = ref 0 in
      fun () -> incr counter; "sulci_fcgi_" ^ string_of_int !counter

let send_query query =
   let inet_addr = inet_addr_any in
   let sock_addr = Unix.ADDR_INET (inet_addr, port) in
   let in_chan, out_chan = Unix.open_connection sock_addr in
      output_value out_chan query;
      flush out_chan;
      input_value in_chan
	       
let make_field varname value =
   Xmlelement ("field", ["var", varname], [make_simple_cdata "value" value])

let display_item out item =
   let fields = get_subels item ~tag:"field" in
      List.iter (fun f ->
		    let field = get_attr_s f "var"
		    and value = get_cdata f ~path:["value"] in
		       if value <> "" then
			  out (field ^ ": " ^ text value ^ "<br>\n")
		) fields

let generate_vsearch (cgi:cgi_activation) =
   let out = cgi # output # output_string in
      begin_page out;
      let fields = List.map (fun field ->
				make_field field (cgi # argument_value field)
			    ) field_list in

      let result = send_query 
	 (Xmlelement ("iq", [("to", "jud.jabber.ru"); 
			     ("type", "set");
			     ("id", new_id ())],
		      [Xmlelement ("query", ["xmlns", "jabber:iq:search"],
				   [Xmlelement ("x", ["xmlns", "jabber:x:data";
						      "type", "submit"],
						fields)])])) in
      let items = get_subels result ~path:["query";"x"] ~tag:"item" in
	 if items = [] then
	    out "<P>No results found</P>\n"
	 else begin
	    out "<P>The result is:</P>\n";
	    List.iter (fun i -> 
			  display_item out i;
			  out "<hr>\n"
		      ) items;
	 end;
	 end_page out
	    
let generate_page (cgi:cgi_activation) =
   match cgi # argument_value "form_id" with
      | ""
      | "vsearch" ->
           generate_vsearch cgi
      | _ ->
           assert false

let process (cgi : cgi_activation) =
   try
      cgi # set_header ~cache:`No_cache 
	 ~content_type:"text/html; charset=UTF-8" ();

      generate_page cgi;

      cgi # output # commit_work();
   with
      | error ->
	   cgi # output # rollback_work();

	   cgi # set_header  ~status:`Internal_server_error ~cache:`No_cache 
              ~content_type:"text/html; charset=UTF-8" ();

           begin_page cgi # output # output_string;
           cgi # output # output_string "Try again later";
           end_page cgi # output # output_string;

           cgi # output # commit_work()

let _= 
   serv process buffered_transactional_optype
