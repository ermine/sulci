(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Dbm
open Xml
open Common
open Hooks

let tlds = 
  let name = try
    get_attr_s Config.config ~path:["plugins"; "tld"] "db"
  with Not_found -> "tlds" in
    try
      opendbm name [Dbm_rdonly] 0o666
    with Dbm_error _ ->
      Printf.eprintf "Cannot open db file %s for plugin_tlds\n" name;
      Pervasives.exit 127
        
let tld text from xml lang out =
  if text = "" then
    make_msg out xml "какой домен?"
  else
    if text = "*" then
      make_msg out xml "http://www.iana.org/cctld/cctld-whois.htm"
    else
      let tld = if text.[0] = '.' then string_after text 1 else text in
        make_msg out xml (try Dbm.find tlds tld with Not_found -> 
                            "неизвестный домен")
          
let _ =   
  register_command "tld" tld
