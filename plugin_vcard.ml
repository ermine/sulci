(*
 * (c) 2005-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open Xmpp
open Jid
open Types
open Common
open Hooks

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
  let success text entity lang xml =
    let rec aux_scan tail acc =
      match tail with
        | [] -> acc
        | h :: t ->
            match h with
              | Xmlelement _ ->
                  let name = get_tagname h in
                  let value = get_cdata h in
                    if trim(value) <> "" then
                      aux_scan t 
                        ((name, value)::acc)
                    else
                      aux_scan t acc
              | _ -> aux_scan t acc
    in
    let res = aux_scan (try get_subels xml ~path:["vCard"] 
                        with Not_found -> []) [] in
      result_vcard res
  in
    Iq.simple_query_entity success ~query_tag:"vCard" "vcard-temp"
      
let _ =
  register_command "vcard" vsearch
    
