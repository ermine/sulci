(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open XMPP
open Common
open Hooks
open Plugin_command

let dns xmpp env kind jid_from text =
  if text = "" then
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_misc_dns_invalid_syntax" [])
  else
    env.env_message xmpp kind jid_from
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
      
let shell xmpp env kind jid_from text =
  if env.env_check_access jid_from "admin" then
    if text = "" then
      env.env_message xmpp kind jid_from "shell? yeah!"
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
          env.env_message xmpp kind jid_from (String.concat "\n" lines);
  else
    env.env_message xmpp kind jid_from "no access"

let plugin opts =
  add_commands [("dns", dns); ("sh", shell)] opts

let _ =
  add_plugin "misc" plugin
