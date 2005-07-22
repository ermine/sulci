(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xml
open Xmlstring
open Getopt

let config =
   let version arg = 
      Printf.printf 
	 "Sulci %s (c) 2004-2005, Anastasia Gornostaeva <ermine@ermine.pp.ru>\n"
	 Version.version;
      Pervasives.exit 0
   in
   let configfile = ref "./sulci.conf" in
   let opts = ["v", "version", OptEmpty, "Show version", Some version;
	       "c", "config", OptString !configfile, "Path to config file", 
	       None] in
   let parsed = Getopt.parse ~help:true opts in
      List.iter (fun (a,b) ->
		    match a with
		       | Shortopt "c"
		       | Longopt "config" ->
			    (match b with
				| OptString str -> configfile := str
				| _ -> ())
		       | _ -> ()
		) parsed;

      if not (Sys.file_exists !configfile) then begin
	 Printf.eprintf "Cannot find a configuration file: %s\n" !configfile;
	 Pervasives.exit 127
      end
      else
	 let fin = open_in !configfile in
	 let rec cycle () =
	    try
	       let line = input_line fin in
		  line ^ cycle ()
	    with _ -> ""
	 in
	 let content = cycle () in
	    Xmlstring.parse_string content
	 
