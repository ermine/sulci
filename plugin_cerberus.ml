(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xml
open Xmpp
open Common
open Muc
open Hooks

let regexp ca          = 0x430 | 0x410 | 'a' | 'A'
let regexp cb          = 0x431 | 0x411
let regexp cv          = 0x432 | 0x412
let regexp cg          = 0x433 | 0x413
let regexp cd          = 0x434 | 0x414
let regexp cie         = 0x435 | 0x415 | 'e' | 'E'
let regexp czh         = 0x436 | 0x416
let regexp cz          = 0x437 | 0x417 | '3'
let regexp ci          = 0x438 | 0x418 | "|/|"
let regexp cj          = 0x439 | 0x419
let regexp ck          = 0x43A | 0x41A
let regexp cl          = 0x43B | 0x41B
let regexp cm          = 0x43C | 0x41C
let regexp cn          = 0x43D | 0x41D | 'H'
let regexp co          = 0x43E | 0x41E | 'o' | 'O' | '0'
let regexp cp          = 0x43F | 0x41F
let regexp cr          = 0x440 | 0x420 | 'p' | 'P'
let regexp cs          = 0x441 | 0x421 | 'c' | 'C'
let regexp ct          = 0x442 | 0x422 | 'T'
let regexp cu          = 0x443 | 0x423 | 'y' | 'Y'
let regexp cf          = 0x444 | 0x424
let regexp ch          = 0x445 | 0x425 | 'x' | 'X'
let regexp cts         = 0x446 | 0x426
let regexp cch         = 0x447 | 0x427
let regexp csh         = 0x448 | 0x428
let regexp cshch       = 0x449 | 0x429
let regexp chard_sign  = 0x44A | 0x42A
let regexp cy          = 0x44B | 0x42B
let regexp csoft_sign  = 0x44C | 0x42C
let regexp ce          = 0x44D | 0x42D
let regexp cyu         = 0x44E | 0x42E
let regexp cya         = 0x44F | 0x42F
let regexp cio         = 0x451 | 0x401 | ci co | cj co

let regexp cyrillic = [0x410-0x44F 0x451 0x401 '0' '3' 'a''A' 'e' 'E' 'H' 
			  'o' 'O' 'c' 'C' 'T' 'x' 'X' 'y' 'Y' 'p' 'P'] | "|/|"

let regexp ci_ie_io = ci | cie | cio
let regexp cie_io = cie | cio

let regexp prefix = cn ca | cn cie | cn ci | cp co | co | ca | cv 
   | cp cr ci | cz ca | cd co | ci cs | cp ce cr ce | cr ca cs | cr ca cz
   | cp cr co | cp cie cr cie | cn cie cd co | cv cy
   | cs chard_sign | cv chard_sign | cs csoft_sign | co ct chard_sign
   | co cd cn co | cn ce cd co 

type t =
   | Bad of string
   | Good

let rec analyze = lexer
   | (cv co)* cb ->
	blja (Ulexing.utf8_lexeme lexbuf) lexbuf
   | prefix* cd ->
	drochit (Ulexing.utf8_lexeme lexbuf) lexbuf
   | cd co cl cb co cie_io cb (* cyrillic* *) ->
	Bad (Ulexing.utf8_lexeme lexbuf)
   | prefix* cm ca cn cd cyrillic ->
	Bad (Ulexing.utf8_lexeme lexbuf)
   | cm ca cn cd cyrillic cyrillic ->
	Good
   | ci cp ca (ct | cn) ->
	Bad (Ulexing.utf8_lexeme lexbuf)
   | ci_ie_io cp ct cyrillic ->
	Bad (Ulexing.utf8_lexeme lexbuf)	
   | cg ca cn cd co cn ->
	Bad (Ulexing.utf8_lexeme lexbuf)	
   | cn ci ci cb ca ->
	Bad (Ulexing.utf8_lexeme lexbuf)
   | co ct cs co cs (ci | (ca ct)) ->
	Bad (Ulexing.utf8_lexeme lexbuf)
   | cyrillic* cs ck ci cp ci cd ca cr (* cyrillic* *) ->
	Good
   | cs cr ca (ck | ct) ->
	Bad (Ulexing.utf8_lexeme lexbuf)
   | cs cs ca cn cyrillic+ ->
	Bad (Ulexing.utf8_lexeme lexbuf)
   | cs cs ca ct (* cyrillic* *) ->
	Bad (Ulexing.utf8_lexeme lexbuf)
   | cv cie cb (* cyrillic* *) ->
	Good
   | cyrillic* czh co cp cyrillic cyrillic ->
	Bad (Ulexing.utf8_lexeme lexbuf)
   | czh co cp cyrillic ->
	Bad (Ulexing.utf8_lexeme lexbuf)
   | ce cp ci cd cie cr ->
	Good
   | cyrillic* ce cb cn cu (* cyrillic* *) ->
	Bad (Ulexing.utf8_lexeme lexbuf)
   | cyrillic* ce cb cu ct (* cyrillic* *) ->
	Bad (Ulexing.utf8_lexeme lexbuf)
   | cyrillic* cp ci cd (co | ca) cr ->
	Bad (Ulexing.utf8_lexeme lexbuf)
   | cyrillic* cp ci cd cie_io cr ->
	Bad (Ulexing.utf8_lexeme lexbuf)
   | prefix* cie_io ->
	ebat (Ulexing.utf8_lexeme lexbuf) lexbuf 
  | prefix cya ->
	ebat (Ulexing.utf8_lexeme lexbuf) lexbuf
   | prefix* cm ->
	mudak (Ulexing.utf8_lexeme lexbuf) lexbuf
   | prefix* ch ->
	xui (Ulexing.utf8_lexeme lexbuf) lexbuf
   | cyrillic* ch cu cie_io cv (* cyrillic* *) ->
	Bad (Ulexing.utf8_lexeme lexbuf)
   | cyrillic* cp ->
	pizda (Ulexing.utf8_lexeme lexbuf) lexbuf
   | cyrillic+ ch cu (cj | ci) cs ->
	Bad (Ulexing.utf8_lexeme lexbuf)
   | cyrillic+ cie_io cb cu cch ->
	Bad (Ulexing.utf8_lexeme lexbuf)
   | cyrillic ->
	skip lexbuf
   | eof ->
	Good
   | _ -> 
	analyze lexbuf

and perdet buf = lexer
   | ce ct ->
	Bad (buf ^ (Ulexing.utf8_lexeme lexbuf))
   | cu cn (* cyrillic* *) ->
	Bad (buf ^ (Ulexing.utf8_lexeme lexbuf))
   | "" ->
	skip lexbuf

and drochit buf = lexer
   | cr co cch ->
	Bad (buf ^ (Ulexing.utf8_lexeme lexbuf))
   | "" ->
	skip lexbuf

and ebat buf = lexer
   | cb ->
	Bad (buf ^ (Ulexing.utf8_lexeme lexbuf))
   | cb ci cl cd ->
	Good
   | "" ->
	skip lexbuf

and mudak buf = lexer
   | cu cd ca (ck | cts) ->
	Bad (buf ^ (Ulexing.utf8_lexeme lexbuf))
   | cu cd ca ct ->
	Bad (buf ^ (Ulexing.utf8_lexeme lexbuf))
   | cu cd ( ca | co) ch ->
	Bad (buf ^ (Ulexing.utf8_lexeme lexbuf))
   | cu cd ci cl (* cyrillic* *) ->
	Bad (buf ^ (Ulexing.utf8_lexeme lexbuf))
   | "" ->
	skip lexbuf

and pizda buf = lexer
   | ci_ie_io cz cd ->
	Bad (buf ^ (Ulexing.utf8_lexeme lexbuf))
   | cie cr cd (ci | cie) (* cyrillic* *) ->
	Bad (buf ^ (Ulexing.utf8_lexeme lexbuf))
   | cie cr cd cu cn (* cyrillic* *) ->
	Bad (buf ^ (Ulexing.utf8_lexeme lexbuf))
   | (ci | cie) cd cr (ci | cie) ->
	Bad (Ulexing.utf8_lexeme lexbuf)
   | "" ->
	skip lexbuf

and blja buf = lexer
   | cl cya ->
	Bad (buf ^ (Ulexing.utf8_lexeme lexbuf))
   | cl cya ch ca ->
	Good
   | cl cya cm cb ->
	Good
   | "" ->
	skip lexbuf

and xui buf = lexer
   | cu (cj | cie) ->
	Bad (buf ^ (Ulexing.utf8_lexeme lexbuf))
   | cu (cya | cio) ->
	 Bad (buf ^ (Ulexing.utf8_lexeme lexbuf))
   | cu ci ->
	Bad (Ulexing.utf8_lexeme lexbuf)
(*
   | cu cl ->
	Bad (Ulexing.utf8_lexeme lexbuf)
   | cu cl ci ct ->
	Good
   | cu cl ci csh ->
	Good
   | cu cl ci cg ca ->
	Good
*)
   | "" ->
	skip lexbuf

and skip = lexer
   | cyrillic* ->
	analyze lexbuf

let report word who phrase out =
   out (Xmlelement ("message", ["type", "chat";
				"to", "ermine@jabber.ru"],
		    [make_simple_cdata "body"
			(Printf.sprintf "Мат: %s\n%s %s\n%s" 
			    word room nick phrase)]))

let kill room nick out =
   if nick = "" then ()
   else
   let id = Hooks.new_id () in
      out (Muc.kick id room nick ("plugin_markov_kick_reason", []));
      let proc x o = 
	 match get_attr_s x "type" with
	    | "error" ->
		 let err_text = try
		    get_error_semantic x
		 with Not_found -> 
(*
		    Lang.get_msg ~xml 
		       "plugin_markov_kick_error" []
*)
		    "hmm.."
		 in
		    out (Xmlelement ("message", ["type", "groupchat";
						 "to", room],
				     [make_simple_cdata "body" err_text]))
	    | _ -> ()
      in
	 Hooks.register_handle (Hooks.Id (id, proc))
		       
let check text room nick out =
   let lexbuf = Ulexing.from_utf8_string text in
      try match analyze lexbuf with
	 | Good -> Good
	 | Bad word ->
	      report word room nick text out;
	      kill room nick out;
	      (* out (Xmlelement ("message", ["to", room;
		 "type", "groupchat"],
		 [make_simple_cdata "body" nick]))
	      *)
	      raise Hooks.FilteredOut
      with
	 | Ulexing.Error ->
	      Printf.printf
		 "Lexing error at offset %i\n" 
		 (Ulexing.lexeme_end lexbuf);
	      flush Pervasives.stdout

let topic = ref " "

let cerberus room event xml out =
   let author = get_resource (get_attr_s xml "from") in
   let room_env = GroupchatMap.find room !groupchats in
      if author <> room_env.mynick then
	 match event with
	    | MUC_join (nick, item) ->
		 if check nick room nick out = Good then
		    check item.status room nick out
	    | MUC_change_nick (nick, _, item) ->
		 check nick room nick out
	    | MUC_presence (nick, item) ->
		 check item.status room nick out
	    | MUC_topic (nick, subject) ->
		 begin 
		    try check subject room author out;
		       topic := subject
		    with Hooks.FilteredOut ->
		       out (Muc.set_topic room !topic);
		       raise FilteredOut
		 end
	    | MUC_message (nick, body) ->
		 if body <> "" then
		    check body room author out
	    | MUC_history ->
		 if get_tagname xml = "message" then begin
		    try
		       let subject = get_cdata xml ~path:["subject"] in
		       let lexbuf = Ulexing.from_utf8_string subject in
			  try match analyze lexbuf with
			     | Good -> topic := subject
			     | Bad word -> ()
			  with
			     | Ulexing.Error ->
				  Printf.printf
				     "Lexing error at offset %i\n" 
			       (Ulexing.lexeme_end lexbuf);
				  flush Pervasives.stdout;
		    with _ -> ()
		 end
	    | _ -> ()

let filter_cerberus xml out =
   let from = get_attr_s xml "from" in
   let room = get_bare_jid from in
      if GroupchatMap.mem room !groupchats then
	 let nick = get_resource from in
	 let event = 
	    if get_tagname xml = "presence" then
	       Muc.process_presence room nick xml out
	    else
	       if get_tagname xml = "message" then
		  Muc.process_message room nick xml out
	       else
		  MUC_other
	 in
	    cerberus room event xml out

let _ =
   (* Muc.register_handle cerberus *)
   Hooks.register_handle (Filter filter_cerberus)
      
