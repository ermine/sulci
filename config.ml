(*                                                                          *)
(* (c) 2004, Anastasia Gornostaeva. <ermine@ermine.pp.ru>                   *)
(*                                                                          *)

open Xml
open Xmlstring

let config = 
   let filename = "sulci.conf" in
   let fin = open_in filename in
   let rec cycle () =
      try
	 let line = input_line fin in
	    line ^ cycle ()
      with _ -> ""
   in
   let content = cycle () in
      Xmlstring.parse_string content
	 
