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
          
let logger_options =
  try 
    let el = Xml.get_tag config ["log"] in
      List.map (fun el -> get_tagname el, get_cdata el)
        (List.find_all (function | Xmlelement _ -> true | _ -> false)
           (get_subels el))
  with Not_found -> []

let log =
  let report_log =
    try List.assoc "report" logger_options with Not_found -> "/dev/null" in
    new Logger.logger ~max_level:"debug" 
      (* ~destination:(new syslog "local0") () in *)
      ~destination:(new Logger.logfile report_log) ()


