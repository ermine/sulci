(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Unix
open Pcre
open Netconversion
open Light_xml
open Common
open Hooks
open Plugin_command

exception Recursive

let load_idx () =
  ()

let dict = ref "myeller_file"
let idx = dict ^ ".hash"
let hash = 
  let fhash = try open_in idx with Sys_error err ->
    raise PluginError "Cannot open Mueller Dictonary's hash file: %s" err;
  in
  let rec aux_read acc =
    let line = try Some (input_line fhash) with End_of_file -> None in
      match line with
        | None -> close_in fhash; List.rev acc
        | Some l -> aux_read l :: acc
  in 
    Array.of_list (aux_read [])
let hash_length = Array.length hash
        
let match_word line word =
  let spword = word ^ "  " in
  let len = String.length spword in
  let rec cycle j =
    if j = len then true
    else if line.[j] = spword.[j] then
      cycle (j+1)
    else if Char.code line.[j] > Char.code spword.[j] then
      false
    else
      raise Recursive
  in
    cycle 0

let mueller_search stuff =
  let fdict = 
    try openfile dict [O_RDONLY] 0o644 
    with Unix_error (err, _, _) ->
      log#crit "plugin_mueller.ml: Cannot open %s: %s"
        dict (Unix.error_message err);
      Pervasives.exit 127
  in
  let rec cycle i =
    if i = hash_length then 
      raise Not_found
    else
      if String.sub stuff 0 2 = String.sub hash.(i) 0 2 then
        let _ = lseek fdict 
          (int_of_string (string_after hash.(i-1) 2)) SEEK_SET 
        in
        let in_fdict = in_channel_of_descr fdict in
        let rec cycle2 () =
          let line = input_line in_fdict in
            try
              if match_word line stuff then
                line
              else
                raise Not_found
            with Recursive -> 
              cycle2 ()
        in
          cycle2 ()
      else
        cycle (i+1)
  in
    cycle 0
        
let mueller xmpp env kind jid_from text =
  if text = "" then
    env.env_message xmpp kind jid_from "гы! Что бум переводить?"
  else
    let reply = 
      try
        let resp = mueller_search text in
        let r = convert ~in_enc:`Enc_koi8r 
          ~out_enc:`Enc_utf8 resp in
        let r1 = regexp "(.+)  \\[.+\\](.+)$" in
        let rsp1 = 
          try
            let r = exec ~rex:r1 r in
              "\n" ^ (get_substring r 1) ^ ": " ^
                (get_substring r 2)
          with Not_found -> "\n" ^ r in
        let r2 = regexp "_([IXV]+)" in
        let rsp2 = substitute_substrings ~rex:r2
          ~subst:(fun a -> "\n" ^ get_substring a 1) rsp1 in
        let r3 = regexp "([0-9]\\.)" in
        let rsp3 = substitute_substrings ~rex:r3 
          ~subst:(fun a -> "\n   " ^ get_substring a 1) rsp2 in
        let r4 = regexp "([a-z]|[0-9]+|_[IVX]+)>" in
        let rsp4 = substitute_substrings ~rex:r4 
          ~subst:(fun a ->  "\n      " ^ 
                    get_substring a 1 ^ ")") rsp3 in
        let r5 = regexp ~iflags:(cflags [`UTF8]) 
          "([абвгдежзийклмно])>" in
        let rsp5 = substitute_substrings ~rex:r5 
          ~subst:(fun a -> "\n         " ^ 
                    get_substring a 1 ^ ")") rsp4 in
          rsp5
      with Not_found -> "Не нашёл :("
    in
      env.env_message xmpp kind jid_from reply
in
  register_command "mueller" mueller
    
let plugin opts =
  let file = "mueller_file" in
    if not (Sys.file_exists f) then
      raise PluginError "Mueller dictonary %s does not exist" file;
    
let _ =
  add_plugin "mueller" plugin
