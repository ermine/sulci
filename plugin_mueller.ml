(*
 * (c) 2004-2010 Anastasia Gornostaeva
 *)

open Unix
open Pcre
open Netconversion
open Hooks
open Plugin_command

type m =
  | This
  | Next
  | Back

let match_item line item =
  let len = String.length item in
  let rec aux_match j =
    if j = len then
      if line.[j] = ' ' && line.[succ j] = ' ' then
        This
      else
        Back
    else if line.[j] = item.[j] then
      aux_match (succ j)
    else if Char.code line.[j] > Char.code item.[j] then
      Back
    else
      Next
  in
    aux_match 0

let mueller_search dict idx stuff =
  let shift =
    if String.length stuff > 1 then
      Hashtbl.find idx (String.sub stuff 0 2)
    else
      Hashtbl.find idx (stuff ^ " ")
  in
  let _ = lseek dict shift SEEK_SET in
  let in_dict = in_channel_of_descr dict in
  let rec aux_read () =
    let line = input_line in_dict in
      match match_item (String.lowercase line) stuff with
        | This -> line
        | Next ->
            aux_read ()
        | Back ->
            raise Not_found
  in
    aux_read ()

let format_response text =
  let r = convert ~in_enc:`Enc_koi8r ~out_enc:`Enc_utf8 text in
  let r1 = regexp "_([IXV]+)" in
  let rsp1 = substitute_substrings ~rex:r1
    ~subst:(fun a -> "\n" ^ get_substring a 1) r in
  let r2 = regexp "([0-9]\\.)" in
  let rsp2 = substitute_substrings ~rex:r2 
    ~subst:(fun a -> "\n   " ^ get_substring a 1) rsp1 in
  let r3 = regexp "([a-z]|[0-9]+|_[IVX]+)>" in
  let rsp3 = substitute_substrings ~rex:r3 
    ~subst:(fun a ->  "\n      " ^ get_substring a 1 ^ ")") rsp2 in
  let r4 = regexp ~iflags:(cflags [`UTF8]) 
    "([абвгдежзийклмно])>" in
  let rsp4 = substitute_substrings ~rex:r4 
    ~subst:(fun a -> "\n         " ^ get_substring a 1 ^ ")") rsp3 in
    rsp4

let mueller dict idx xmpp env kind jid_from text =
  if text = "" then
    env.env_message xmpp kind jid_from "гы! Что бум переводить?"
  else
    let reply = 
      try
        let resp = mueller_search dict idx (String.lowercase text) in
          format_response resp
      with Not_found -> "Не нашёл :("
    in
      env.env_message xmpp kind jid_from reply
    
let rec read_file f shift saved_part acc =
  let line = try Some (input_line f) with End_of_file -> None in
    match line with
      | None -> List.rev acc
      | Some l ->
          let part = String.lowercase (String.sub l 0 2) in
          let newshift = shift + String.length l + 1 in
            if part = saved_part then
              read_file f newshift saved_part acc
            else
              read_file f newshift part ((part, shift) :: acc)

let plugin opts =
  let file =
    try List.assoc "file" (List.assoc "db" opts)
    with Not_found ->
      raise (Plugin.Error
  "Please specify <db file='/path/Mueller.koi'/> element in configuration file"
            ) in
  let () =
    if not (Sys.file_exists file) then
      raise (Plugin.Error (Printf.sprintf "%s does not exists" file)) in
  let hdict = open_in_bin file in
  let idxs = read_file hdict 0 "  " [] in
  let idx = Hashtbl.create (List.length idxs) in
  let () =
    close_in hdict;
    List.iter (fun (part, shift) -> Hashtbl.add idx part shift) idxs;
  in
    add_for_token
      (fun _opts xmpp ->
         let dict = 
           try openfile file [O_RDONLY] 0o644 
           with Unix_error (err, _, _) ->
             raise (Plugin.Error ("Cannot open " ^ file))
         in
           Plugin_command.add_commands xmpp
             [("mueller", mueller dict idx)] opts
      )
    
let () =
  Plugin.add_plugin "mueller" plugin
