open Xml
open Xmlstring

let conf = 
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
	 
