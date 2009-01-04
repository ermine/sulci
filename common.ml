(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open Jid

let string_after s n =
  String.sub s n (String.length s - n)

let skip_ws str =
  if str = "" then str
  else
    let rec cycle i =
      if i = String.length str then ""
      else
        if List.mem str.[i] [' '; '\n'; '\r'; '\t'] then cycle (succ i)
        else if i > 0 then string_after str i
        else str
    in
      cycle 0
        
let rskip_ws str =
  if str = "" then str
  else
    let rec cycle i =
      if i = -1 then ""
      else
        if List.mem str.[i] [' '; '\n'; '\t'; '\r'] then cycle (pred i)
        else String.sub str 0 (i+1)
    in
      cycle (pred (String.length str))
        
let trim str =
  let r1 = skip_ws str in
    rskip_ws r1
      
let msg_limit = ref 
  (try int_of_string (get_attr_s Config.config ~path:["muc"] "msg_limit")
   with Not_found -> 450)

let max_message_length = ref
  (try int_of_string
     (get_attr_s Config.config ~path:["max_message_length"] "value")
   with Not_found -> 10000)
  
exception InvalidUTF8

let sub_utf8_string text count =
  let len = String.length text in
  let rec aux_sub i l =
    if l = count then
      String.sub text 0 i
    else if i < len then
      match text.[i] with
        | '\000' .. '\127' ->
            aux_sub (i+1) (l+1)
        | '\193' .. '\223' ->
            aux_sub (i+2) (l+1)
        | '\224' .. '\239' ->
            aux_sub (i+3) (l+1)
        | '\240' .. '\248' ->
            aux_sub (i+4) (l+1)
        | _ -> raise InvalidUTF8
    else
      text
  in
    aux_sub 0 0
      
let sub_utf8 text length =
  let rec aux_sub i j =
    if i < 0 then
      0
    else
      match text.[i] with
        | '\000' .. '\127' -> i
        | '\193' .. '\223' ->
            if j = 1 then i else aux_sub (i-1) 0
        | '\224' .. '\239' ->
            if j = 2 then i else aux_sub (i-1) 0
        | '\240' .. '\248' ->
            if j = 3 then i else aux_sub (i-1) 0
        | _ ->
            aux_sub (i-1) (j+1)
  in
    if String.length text <= length then
      text
    else
      let len = aux_sub (length - 1) 0 in
        if len > 0 then String.sub text 0 len else ""
          
let clean_tail str =
  if str = "" then str
  else
    let rec cycle i =
      if i = -1 then str
      else
        if Char.code str.[i] < 128 &&
          List.mem str.[i] [' '; '\n'; '\t'; '\r'; ','; '.'; ':'; ';'] then
            String.sub str 0 (i+1)
        else
          cycle (pred i)
    in
      cycle (pred (String.length str))
        
let split_long_message limit msg tail =
  let rec aux_split acc rest =
    if String.length rest <= limit then
      if String.length rest + String.length tail <= limit then
        List.rev ((rest ^ tail) :: acc)
      else
        List.rev (tail :: rest :: acc)
    else
      let part = sub_utf8 rest limit in
      let cleaned = clean_tail part in
      let new_tail = string_after rest (String.length cleaned) in
        aux_split (cleaned :: acc) new_tail
  in
    aux_split [] msg
      
let make_msg out xml ?response_tail response =
  let from = jid_of_string (Xml.get_attr_s xml "from") in
  let nick = from.resource in
  let tail =
    match response_tail with
      | None -> ""
      | Some t -> "\n" ^ t
  in
    match safe_get_attr_s xml "type" with
      | "groupchat" ->
          let limit = 
            let l = !msg_limit - String.length tail in
              if l < 0 then 0 else l in
          let resp = sub_utf8_string response limit in
          let cutted, respo =
            if String.length resp < String.length response then
              true, clean_tail resp ^ "[...]" ^ tail
            else 
              false, resp ^ tail
          in
            out (make_element "message" ["to", string_of_jid (bare_jid from);
                                         "type", "groupchat"]
                   [make_simple_cdata "body" 
                      (if Pcre.pmatch ~pat:"/me" response then
                         respo
                       else
                         if nick = "" then respo
                         else (nick ^ ": " ^ respo)
                      )]);
            if cutted then
              let msgs = split_long_message !max_message_length response tail in
                List.iter (fun m ->
                             out (make_element "message"
                                    ["to", from.string; "type", "chat"]
                                    [make_simple_cdata "body" m])) msgs
       | other ->
          let msgs = split_long_message !max_message_length response tail in
            List.iter (fun m ->
                         out (make_element "message"
                                (match other with
                                   | "" -> ["to", from.string]
                                   | o -> ["to", from.string; "type", other])
                                [make_simple_cdata "body" m])) msgs

(* temp code *)
exception DNSPrepError
  
let dnsprep str =
  if String.contains str '.' then ()
  else raise DNSPrepError
    
