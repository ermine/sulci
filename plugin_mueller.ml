open Unix
open Types
open Netconversion

exception Recursive

let dict = "/usr/local/share/mueller-dic/Mueller7accentGPL.koi"
let idx = dict ^ ".h"
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
		       (int_of_string (Str.string_after hash.(i-1) 2)) SEEK_SET 
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

let rex = Str.regexp "mueller \\([^\n]+\\)"

let mueller xml out bot mynick lang =
   let body = try Xml.get_cdata xml ~path:["body"] with _ -> "" in
      if Str.string_match rex body 0 then
	 let word = Str.matched_group 1 body in
	 let reply = 
	    try
	       let response = mueller_search word in
	       let r1 = Str.regexp "\\(.+\\)  \\[.+\\]\\(.+\\)$" in
	       let rsp1 = if Str.string_match r1 response 0 then
		  "\n" ^ (Str.matched_group 1 response) ^ ": " ^
		  (Str.matched_group 2 response)
	       else "\n" ^ response in
	       let r2 = Str.regexp "\\([0-9]\\.\\)" in
	       let rsp2 = Str.global_substitute r2
			     (function x ->
				 let z = Str.matched_group 1 rsp1 in
				    "\n   " ^ z
			     ) rsp1 in
	       let r3 = Str.regexp "\\([a-z]\\|[0-9]+\\|_[IVX]+\\)>" in
	       let rsp3 = Str.global_substitute r3
			     (function x ->
				 let z = Str.matched_group 1 rsp2 in
				    "\n      " ^ z ^ ")"
			     ) rsp2 in
	       let r4 = Str.regexp "\\([абвгдежзийклмно]\\)>" in
	       let rsp4 = Str.global_substitute r4
			     (function x ->
				 let z = Str.matched_group 1 rsp3 in
				    "\n         " ^ z ^ ")"
			     ) rsp3 in
		  convert ~in_enc:`Enc_koi8r ~out_enc:`Enc_utf8 rsp4
	    with Not_found -> "Не нашёл :("
	 in
	    out (make_msg xml reply)


let _ =
   Muc.register_cmd "mueller" mueller
