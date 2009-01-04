(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Unix
open Types
open Common
open Hooks

let dns text from xml env out =
  if text = "" then
    make_msg out xml
      (Lang.get_msg env.env_lang "plugin_misc_dns_invalid_syntax" [])
  else
    make_msg out xml
      (try
         let inet_addr = inet_addr_of_string text in
         let s = gethostbyaddr inet_addr in
           s.h_name ^ " " ^
             (String.concat " "
                (Array.to_list s.h_aliases))
       with 
         | Failure _ ->
             (try 
                let h = gethostbyname text in
                let hl = Array.to_list h.h_addr_list in
                  String.concat " "
                    (List.map (fun addr -> string_of_inet_addr addr) hl)
              with Not_found ->  
                Lang.get_msg env.env_lang "plugin_misc_dns_not_resolved" []
             ) 
         | Not_found ->
             Lang.get_msg env.env_lang "plugin_misc_dns_not_resolved" [])
      

let _ =
  register_command "dns" dns
