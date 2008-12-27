(*
 * (c) 2004-2008 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Unix
open Common

let dns text from xml lang out =
  if text = "" then
    make_msg out xml (Lang.get_msg lang "plugin_misc_dns_invalid_syntax" [])
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
                Lang.get_msg lang "plugin_misc_dns_not_resolved" []
             ) 
         | Not_found ->
             Lang.get_msg lang "plugin_misc_dns_not_resolved" [])
      
let _ =
  Hooks.register_handle (Hooks.Command ("dns", dns))
