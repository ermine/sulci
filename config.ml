(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Arg
open Xml
open Xmlstring

let version () =
  Printf.printf 
    "%s %s (c) 2004-2009, Anastasia Gornostaeva <ermine@ermine.pp.ru>\n"
    Version.name
    Version.version;
  Pervasives.exit 0

let config =
  let usage_msg = Filename.basename Sys.argv.(0) ^ " [options]" in
  let cfile = ref (Filename.basename Sys.argv.(0) ^ ".conf") in
  let opts = align [
    "-c", Set_string cfile, "<file>  Path to the config file";
    "-v", Unit version, " Show version";
  ] in
  let () = Arg.parse opts
    (fun unk -> Printf.eprintf "Unknown option %S\n" unk;
       usage opts usage_msg;
       Pervasives.exit 1)
    usage_msg in
    
    if not (Sys.file_exists !cfile) then (
      Printf.eprintf "Cannot find a configuration file: %s\n" !cfile;
      Pervasives.exit 127
    )
    else
      let xml = Xmlstring.parse_from_file !cfile in
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

let acls =
  let acls = get_subels config ~tag:"acl" in
    List.map (fun a ->
                Jid.jid_of_string (get_attr_s a "jid"), get_attr_s a "class"
             ) acls
  
