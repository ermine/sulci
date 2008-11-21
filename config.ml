(*
 * (c) 2004-2008 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open Xmlstring
open Getopt

let config =
  let version arg = 
    Printf.printf 
      "%s %s (c) 2004-2005, Anastasia Gornostaeva <ermine@ermine.pp.ru>\n"
      Version.name
      Version.version;
    Pervasives.exit 0
  in
  let configfile = ref "./sulci.conf" in
  let create_account = ref false in
  let opts = ["v", "version", OptEmpty, "Show version", Some version;
  "c", "config", OptString !configfile, "Path to config file", None;
  "r", "register", OptEmpty, "Create an account", None
             ] in
  let parsed = Getopt.parse ~help:true opts in
    List.iter (fun (a,b) ->
                 match a with
                   | Shortopt "c"
                   | Longopt "config" ->
                       (match b with
                          | OptString str -> configfile := str
                          | _ -> ())
                   | Shortopt "r"
                   | Longopt "register" ->
                       create_account := true
                   | _ -> ()
              ) parsed;
    
    if not (Sys.file_exists !configfile) then begin
      Printf.eprintf "Cannot find a configuration file: %s\n" !configfile;
      Pervasives.exit 127
    end
    else
      let xml = Xmlstring.parse_from_file !configfile in
        xml
          
