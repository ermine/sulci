(*
 * (c) 2005-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open XMPP
open Jid
open Types
open Common
open Hooks

let ns_vcard = Some "vcard-temp"

let result_vcard alist =
  if alist = [] then
    "Нет сведений"
  else
    let r1 = try List.assoc "FN" alist with Not_found -> "" in
    let r2 = try 
      (if r1 = "" then "" else r1 ^ " -- ") ^
        List.assoc "DESC" alist with Not_found -> r1 in 
    let r3 = try
      (if r2 = "" then "" else r2 ^ " -- ") ^
        List.assoc "URL" alist with Not_found -> r2 in
    let r4 = try
      (if r3 = "" then "" else r3 ^ " -- ") ^ "Email: " ^
        List.assoc "EMAIL" alist with Not_found -> r3 in
      r4
        
let vsearch = 
  let success _text _entity _lang vcard =
    let res =
      List.fold_left
        (fun acc -> function
           | Xmlelement (qname, _, _) as el ->
               let name = get_name qname in
               let value = get_cdata el in
                 if trim(value) <> "" then
                   (name, value) :: acc
                 else
                   acc
           | Xmlcdata _ ->
               acc
        ) []
        (match vcard with | None -> [] | Some el -> get_children el)
    in
      result_vcard res
  in
    Iq.simple_query_entity success
      ~payload:(make_element (ns_vcard, "vCard") [] []) 
     
let _ =
  register_command "vcard" vsearch
    
