(*
 * (c) 2004-2010 Anastasia Gornostaeva
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
      
let plugin opts =
  add_for_token
    (fun _opts xmpp ->
       add_commands xmpp [("dns", dns)] opts
    )

let _ =
  Plugin.add_plugin "misc" plugin
