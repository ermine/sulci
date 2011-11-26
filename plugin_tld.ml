(*
 * (c) 2004-2010 Anastasia Gornostaeva
 *)

open Dbm
open Common
open Hooks
open Plugin_command

let tld db xmpp env kind jid_from text =
  let response =
    if text = "" then
      "какой домен?"
    else
      if text = "*" then
        "http://www.iana.org/cctld/cctld-whois.htm"
      else
        let tld = if text.[0] = '.' then string_after text 1 else text in
          (try Dbm.find db tld with Not_found -> "неизвестный домен")
  in
    env.env_message xmpp kind jid_from response


let plugin opts =
  let tld_file =
    try List.assoc "file" (List.assoc "db" opts)
    with Not_found ->
      raise
        (Plugin.Error
           "Please specify <db file=/path/tlds'/> element in configuration file")
  in
  let db =
    try opendbm tld_file [Dbm_rdonly] 0o666
    with Dbm_error err ->
      raise (Plugin.Error
               (Printf. sprintf "Cannot open db file %s: %s" tld_file err))
  in
    add_for_token
      (fun _opts xmpp ->
         add_commands xmpp [("tld", (tld db))] opts
      )

let _ =
  Plugin.add_plugin "tld" plugin
