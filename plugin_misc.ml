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
      
let shell text from xml env out =
  if env.env_check_access from "admin" then
    if text = "" then
      make_msg out xml "shell? yeah!"
    else
      let in_ch = Unix.open_process_in text in
      let buf = Buffer.create 80 in
        (try
           while true do
             Buffer.add_string buf (input_line in_ch);
             Buffer.add_char buf '\n'
           done
         with End_of_file -> close_in in_ch);
        if Buffer.length buf > 0 then
          make_msg out xml (Buffer.contents buf);
  else
    make_msg out xml "no access"

let _ =
  register_command "dns" dns;
  register_command "sh" shell
