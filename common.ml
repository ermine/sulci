(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

let string_after s n =
  if n = 0 then
    s
  else if String.length s = n then
    ""
  else
    String.sub s n (String.length s - n)

let is_prefix s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
    if len1 > len2 then
      false
    else
      let rec aux_compare i =
        if i < len1 then
          if s1.[i] = s2.[i] then
            aux_compare (succ i)
          else
            false
        else
          true
      in
        aux_compare 0
    
let skip_ws str i =
  if str = "" then str
  else
    let rec cycle i =
      if i < String.length str then
        if List.mem str.[i] [' '; '\n'; '\r'; '\t'] then cycle (succ i)
        else if i > 0 then string_after str i
        else str
      else
        ""
    in
      cycle i
        
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
  let r1 = skip_ws str 0 in
    rskip_ws r1
      
exception InvalidUTF8

let length_utf8 text =
  let rec aux_sub i j =
    if i < String.length text then
      match text.[i] with
        | '\000' .. '\127' ->
            aux_sub (succ i) (succ j)
        | '\193' .. '\223' ->
            aux_sub (i+2) (succ j)
        | '\224' .. '\239' ->
            aux_sub (i+3) (succ j)
        | '\240' .. '\248' ->
            aux_sub (i+4) (succ j)
        | _ -> raise InvalidUTF8
    else
      j
  in
    aux_sub 0 0
  

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
      
exception Error
  
(* temp code *)
exception DNSPrepError
  
let dnsprep str =
  if String.contains str '.' then ()
  else raise DNSPrepError
