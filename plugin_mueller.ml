(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Unix
open Common
open Xml
open Netconversion
open Pcre

exception Recursive

let dict = get_attr_s Config.config ~path:["plugins"; "mueller"] "file"
let idx = dict ^ (get_attr_s Config.config ~path:["plugins";"mueller"] "hashext")
let hash = 
   let fhash = open_in idx in
   let rec cycle () =
      try 
	 let line = input_line fhash in
	    line :: cycle ()
      with _ -> []
   in 
      Array.of_list(cycle ())
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

   let fdict = openfile dict  [O_RDONLY] 0o644 in

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

let mueller text event from xml out =
   if text = "" then
      out (make_msg xml "гы! Что бум переводить?")
   else
      let reply = 
	 try
	    let resp = mueller_search text in
	    let r = convert ~in_enc:`Enc_koi8r ~out_enc:`Enc_utf8 resp in
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
	       ~subst:(fun a ->  "\n      " ^ get_substring a 1 ^ ")") rsp3 in
	    let r5 = regexp ~iflags:(cflags [`UTF8]) "([абвгдежзийклмно])>" in
	    let rsp5 = substitute_substrings ~rex:r5 
	       ~subst:(fun a -> "\n         " ^ get_substring a 1 ^ ")") rsp4 in
	       rsp5
	 with Not_found -> "Не нашёл :("
      in
	 out (make_msg xml reply)

let _ =
   Hooks.register_handle (Hooks.Command ("mueller", mueller))
