(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Unix
open Pcre
open Xml
open Config
open Common
open Hooks
open Netconversion

exception Recursive

let _ =
  if exists_element "mueller" (get_subels ~path:["plugins"] Config.config) then
    let mueller =
      let dict = try 
        let f = get_attr_s Config.config ~path:["plugins"; 
                                                "mueller"] "file" in
          if Sys.file_exists f then f else (
            log#crit "Mueller dictonary %s does not exists" f;
            Pervasives.exit 127
          )
      with Not_found ->
        log#crit "Invalid mueller stanza in config file";
        Pervasives.exit 127
      in
      let idx =
        dict ^ (try (get_attr_s Config.config ~path:["plugins";"mueller"] 
                       "hashext")
                with Not_found -> ".hash")
      in
      let hash = 
        let fhash = try open_in idx with Sys_error err ->
          log#crit "Cannot open Mueller Dictonary's hash file: %s" err;
          Pervasives.exit 127
        in
        let rec cycle () =
          try 
            let line = input_line fhash in
              line :: cycle ()
          with _ -> close_in fhash; []
        in 
          Array.of_list(cycle ())
      in
      let hash_length = Array.length hash in
        
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
      in
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
      in
        fun text from xml lang out ->
          if text = "" then
            make_msg out xml "гы! Что бум переводить?"
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
              make_msg out xml reply
    in
      register_command "mueller" mueller
        
