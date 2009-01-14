(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open Xmpp
open Jid
open Types
open Config
open Common
open Hooks
open Muc_types
open Muc
open Nicks

let regexp ca          = 0x430 | 0x410 | 'a' | 'A'
let regexp cb          = 0x431 | 0x411
let regexp cv          = 0x432 | 0x412 | 'B' 
let regexp cg          = 0x433 | 0x413
let regexp cd          = 0x434 | 0x414
let regexp cie         = 0x435 | 0x415 | 'e' | 'E'
let regexp czh         = 0x436 | 0x416 | "}|{" | ")|(" | "&gt;|&lt;"
let regexp cz          = 0x437 | 0x417 | '3'
let regexp ci          = 0x438 | 0x418 | "|/|"
let regexp cj          = 0x439 | 0x419
let regexp ck          = 0x43A | 0x41A | 'K' | 'k'
let regexp cl          = 0x43B | 0x41B
let regexp cm          = 0x43C | 0x41C | 'M'
let regexp cn          = 0x43D | 0x41D | 'H'
let regexp co          = 0x43E | 0x41E | 'o' | 'O' | '0'
let regexp cp          = 0x43F | 0x41F | 'n'
let regexp cr          = 0x440 | 0x420 | 'p' | 'P'
let regexp cs          = 0x441 | 0x421 | 'c' | 'C'
let regexp ct          = 0x442 | 0x422 | 'T'
let regexp cu          = 0x443 | 0x423 | 'y' | 'Y'
let regexp cf          = 0x444 | 0x424
let regexp ch          = 0x445 | 0x425 | 'x' | 'X' | "}{" | ")("| "&gt;&lt;"
let regexp cts         = 0x446 | 0x426
let regexp cch         = 0x447 | 0x427
let regexp csh         = 0x448 | 0x428
let regexp cshch       = 0x449 | 0x429
let regexp chard_sign  = 0x44A | 0x42A
let regexp cy          = 0x44B | 0x42B | "bl" | "bI"
let regexp csoft_sign  = 0x44C | 0x42C
let regexp ce          = 0x44D | 0x42D
let regexp cyu         = 0x44E | 0x42E
let regexp cya         = 0x44F | 0x42F
let regexp cio         = 0x451 | 0x401 | ci co | cj co

let regexp pi = '3' ('.' | ',') ['0'-'9']+

let regexp cyrillic = [0x410-0x44F 0x451 0x401 '0' '3' 'a''A' 'e' 'E' 'H' 
                         'o' 'O' 'c' 'C' 'k' 'K' 'T' 'x' 'X' 'y' 'Y' 'p' 
                         'P'] | "|/|" | "bl" | "bI" 

let regexp vowel = ca | co | ce | cie | ci | cu | cy

let regexp ci_ie_io = ci | cie | cio

let regexp cie_io = cie | cio

let regexp prefix = cn ca | cn cie | cn ci | cp co | co | ca | cv 
   | cp cr ci | cz ca | cd co | ci cs | cp ce cr ce | cr ca cs | cr ca cz
   | cp cr co | cp cie cr cie | cn cie cd co | cv cy
   | cs chard_sign | cv chard_sign | cn ce cv chard_sign 
   | cs csoft_sign | co ct chard_sign
   | co cd cn co | cn ce cd co | cs cu cp cie cr | cg ci cp cie cr
         
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
  | cm ca cn cd (vowel | co cj) ->
      Bad (Ulexing.utf8_lexeme lexbuf)
  | cm ca cn cd cyrillic cyrillic ->
      Good
  | ci cp ct ca cb
  | ci cp ct cie (ci | cj)
  | ci cp ct cr ->
      Good
  | ci cp ca ct csoft_sign cie cv cs ck ->
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
  | cp ci cn cd cie cts ->
      Bad (Ulexing.utf8_lexeme lexbuf)
  | cm cl cya (ct | cd) csoft_sign ->
      Bad (Ulexing.utf8_lexeme lexbuf)
  | cyrillic* cs ck ci cp ci cd ca cr (* cyrillic* *) ->
      Good
  | cs cr ca (ck | ct) ->
      Bad (Ulexing.utf8_lexeme lexbuf)
  | cs cp ci cd co cr cm ca cn ->
      Good
  | cs cs ca cn cyrillic+ ->
      Bad (Ulexing.utf8_lexeme lexbuf)
  | cs cs ca ct (* cyrillic* *) ->
      Bad (Ulexing.utf8_lexeme lexbuf)
  | cv cie cb (* cyrillic* *) ->
      Good
  | cp ci cp ci cs csoft_sign ck ->
      Bad (Ulexing.utf8_lexeme lexbuf)
        (*   | cp ci cs cya *)
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
  | cie_io cb cu cr ->
      Good
  | prefix? cie_io ->
      ebat (Ulexing.utf8_lexeme lexbuf) lexbuf 
  | prefix cya ->
      ebat (Ulexing.utf8_lexeme lexbuf) lexbuf
  | prefix? cm ->
      mudak (Ulexing.utf8_lexeme lexbuf) lexbuf
  | prefix? ch ->
      xui (Ulexing.utf8_lexeme lexbuf) lexbuf
  | prefix? cs ct cr ca ch cu cj (cs cya)? ->
      Good
  | prefix? csh ct cr ci ch cu cj (cs cya)? ->
      Good
  | cv? ch cu ci cz ->
      Good
  | cyrillic* ch cu cie_io cv (* cyrillic* *) ->
      Bad (Ulexing.utf8_lexeme lexbuf)
  | pi cz cd ->
      Bad (Ulexing.utf8_lexeme lexbuf)
  | cyrillic* cp ->
      pizda (Ulexing.utf8_lexeme lexbuf) lexbuf
  | cyrillic+ ch cu (cj | ci) cs ->
      Bad (Ulexing.utf8_lexeme lexbuf)
  | prefix* cg cr cie cb cu cch
  | prefix* cch cie cb cu cch ->
      Good
  | cyrillic+ cie_io cb cu cch ->
      Bad (Ulexing.utf8_lexeme lexbuf)
  | cyrillic ->
      skip lexbuf
  | cs cu cp cie cr cd
  | cg ci cp cie cr cd ->
      Good
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
    Bad (buf ^ (Ulexing.utf8_lexeme lexbuf))
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
    Bad (buf ^ (Ulexing.utf8_lexeme lexbuf))
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
      
let notify_jids =
  let jids = try 
    Xml.get_subels ~path:["plugins"; "cerberus"] ~tag:"notify" Config.config 
  with _ -> [] in
    List.map (fun j -> Xml.get_attr_s j "jid") jids
      
let do_kick =
  try if Xml.get_attr_s Config.config ~path:["plugins"; "cerberus"] "kick" =
    "true" then true else false
  with _ -> true
    
let report word from phrase msg_type out =
  let jid_from =
    try
      let item = Nicks.find from.lresource (get_room_env from).nicks in
        match item.jid with
          | None -> "unknown jid"
          | Some j -> j.string
    with Not_found -> "unknown jid"
  in
    List.iter (fun jid ->
                 out (make_message ~to_:jid ~type_:`Chat
                        ~body:(Printf.sprintf 
                                 "Censor: %s
Room: %s@%s
Nick: %s (%s)
[%s] %s"
                                 word
                                 from.lnode from.ldomain
                                 from.resource jid_from
                                 msg_type phrase) ())
              ) notify_jids
      
let kill (from:jid) xml env out =
  if from.resource = "" then ()
  else
    let room = from.lnode, from.ldomain in
    let room_env = get_room_env from in
    let myitem = Nicks.find room_env.mynick room_env.nicks in
      if myitem.role = `Moderator then
        let item = Nicks.find from.lresource room_env.nicks in
          if item.role = `Moderator then
            make_msg out xml (Lang.get_msg env.env_lang
                                "plugin_cerberus_cannot_kick_admin" [])
          else
            let proc t f x o = () in
            let id = new_id () in
            let reason = Lang.get_msg env.env_lang
              "plugin_markov_kick_reason" [] in
              register_iq_query_callback id proc;
              out (Muc.kick ~reason id room from.resource)
      else
        make_msg out xml (Lang.get_msg env.env_lang
                            "plugin_cerberus_cannot_kick_admin" [])

let topics = Hashtbl.create 5
  
let cerberus event from xml env out =
  let check text msg_type =
    let lexbuf = Ulexing.from_utf8_string text in
      try match analyze lexbuf with
        | Good -> ()
        | Bad word ->
            report word from text msg_type out;
            if do_kick then
              kill from xml env out;
            raise Filtered
      with
        | Ulexing.Error ->
            log#error "cerberus: Lexing error at offset %i"
              (Ulexing.lexeme_end lexbuf)
  in
  let room = from.lnode, from.ldomain in
    match event with
      | MUC_join item ->
          if from.lresource <> (get_room_env from).mynick then (
              check from.resource "resource";
              check item.status "status";
              match item.jid with
                | None -> ()
                | Some j ->
                    check j.lresource "jid";
            )
      | MUC_change_nick (nick, item) ->
          if nick <> (get_room_env from).mynick then
            check nick "nick"
      | MUC_presence item ->
          if from.lresource <> (get_room_env from).mynick then
            check item.status "presence"
      | MUC_topic subject ->
          if from.lresource <> (get_room_env from).mynick then (
            try
              check subject "topic";
              Hashtbl.replace topics (from.lnode, from.ldomain) subject;
            with Filtered ->
              let saved_topic =
                try Hashtbl.find topics (from.lnode, from.ldomain) with
                    Not_found -> " " in
                out (Muc.set_topic from saved_topic);
                raise Filtered
          )
      | MUC_message (msg_type, nick, body) ->
          if msg_type <> `Error then
            if body <> "" && from.lresource <> (get_room_env from).mynick then
              check body (match msg_type with
                            | `Groupchat -> 
                                "groupchat public"
                            | _ ->
                                "groupchat private")
      | MUC_history ->
          if get_tagname xml = "message" then (
            try
              let subject = get_cdata xml ~path:["subject"] in
              let lexbuf = Ulexing.from_utf8_string subject in
                try match analyze lexbuf with
                  | Good ->
                      Hashtbl.replace topics (from.lnode, from.ldomain) subject
                  | Bad word -> ()
                with
                  | Ulexing.Error ->
                      log#error "cerberus: Lexing error at offset %i" 
                        (Ulexing.lexeme_end lexbuf);
            with exn ->
              (* log#error "cerberus" exn *) ()
          )
      | _ -> ()
          
let _ = 
  Muc.register_filter "cerberus" cerberus
