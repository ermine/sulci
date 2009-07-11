(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open XMPP
open Types
open Common
open Hooks

let dns text _from xml env out =
  if text = "" then
    make_msg out xml
      (Lang.get_msg env.env_lang "plugin_misc_dns_invalid_syntax" [])
  else
    make_msg out xml
      (try
         let inet_addr = Unix.inet_addr_of_string text in
         let s = Unix.gethostbyaddr inet_addr in
           s.Unix.h_name ^ " " ^
             (String.concat " " (Array.to_list s.Unix.h_aliases))
       with 
         | Failure _ ->
             (try 
                let h = Unix.gethostbyname text in
                let hl = Array.to_list h.Unix.h_addr_list in
                  String.concat " "
                    (List.map (fun addr -> Unix.string_of_inet_addr addr) hl)
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
      let rec aux_read acc =
        let line = try Some (input_line in_ch) with End_of_file -> None in
          match line with
            | Some v -> aux_read (v :: acc)
            | None -> close_in in_ch; List.rev acc
      in
      let lines = aux_read [] in
        if lines <> [] then
          make_msg out xml (String.concat "\n" lines);
  else
    make_msg out xml "no access"

let _ =
  register_command "dns" dns;
  register_command "sh" shell
