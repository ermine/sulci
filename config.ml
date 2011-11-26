(*
 * (c) 2004-2010 Anastasia Gornostaeva
 *)

open Arg
open Light_xml
open JID
open Hooks

type account = {
  jid : JID.t;
  ip : string;
  port : int option;
  password : string;
  resource : string;
  rawxml_log : string;
  reconnect_interval : int;
  reconnect_times : int;
  use_tls : bool;
  use_compress : bool;
  max_stanza_length : int;
  max_message_length : int;
}

let unknown msg =
  Printf.eprintf "%s\n" msg;
  Pervasives.exit 127

let need f x msg =
  try f x with _ ->
    Printf.eprintf "Error in configuration file: %s\n" msg;
    Pervasives.exit 127
  
let parse_account jid els =
  List.fold_left (fun account -> function
                    | Xmlelement (name, attrs, els) as el -> (
                        match name with
                          | "ip" ->
                              {account with ip = get_cdata el}
                          | "port" ->
                              let port = need int_of_string (get_cdata el)
                                "account/port MUST be integer" in
                                {account with port = Some port}
                          | "password" ->
                              {account with password = get_cdata el}
                          | "resource" ->
                              {account with resource = get_cdata el}
                          | "rawxml_log" ->
                              {account with rawxml_log = get_cdata el}
                          | "reconnect_interval" ->
                              let value = need int_of_string (get_cdata el)
                                "account/reconnect_interval MUST be integer" in
                                {account with reconnect_interval = value}
                          | "reconnect_times" ->
                              let value = need int_of_string (get_cdata el)
                                "account/reconnect_times MUST be integer" in
                                {account with reconnect_times = value}
                          | "starttls" ->
                              {account with use_tls = true}
                          | "compress" ->
                              {account with use_compress = true}
                          | "max_stanza_length" ->
                              let value = need (List.assoc "value") attrs
                            "account/max_stanza_lehgth MUST have value attribute"
                              in
                              let i = need int_of_string value
                                "account/max_stanza_length/value MUST be integer"
                              in
                                {account with max_stanza_length = i}
                          | "max_message_length" ->
                              let value = need (List.assoc "value") attrs
                           "account/max_message_lehgth MUST have value attribute"
                              in
                              let i = need int_of_string value
                               "account/max_message_length/value MUST be integer"
                              in
                                {account with max_message_length = i}
                          | _ ->
                              account
                      )
                    | _ ->
                        account
                 ) {jid = jid;
                    ip = "";
                    port = None;
                    password = "";
                    resource = "";
                    rawxml_log = "";
                    reconnect_interval = 10;
                    reconnect_times = 3;
                    use_tls = false;
                    use_compress = false;
                    max_stanza_length = 65536;
                    max_message_length = 400;
                   } els

let setup_logger els =
  let level, dst =
    List.fold_left
      (fun (level, dst) -> function
         | Xmlelement (name, attrs, els) -> (
             match name with
               | "level" ->
                   let value = need (List.assoc "value") attrs
                     "log/level MUST have value attribute" in
                     (value, dst)
               | "syslog" ->
                   let facility = need (List.assoc "facility") attrs
                     "log/syslog MUST have a facility attribute" in
                     (level, Some (new Logger.syslog facility))
               | "stderr" ->
                   (level, Some (new Logger.log_stderr))
               | "piped_log" ->
                   let cmd = need (List.assoc "cmd") attrs
                     "log/piped_log MUST have cmd attribute" in
                     (level, Some (new Logger.piped_log cmd))
               | "file" ->
                   let path = need (List.assoc "path") attrs
                     "log/file MUST have path attribute" in
                     (level, Some (new Logger.logfile path))
               | other ->
                   unknown other
           )
         | Xmlcdata _ ->
             (level, dst)
      ) ("none", None) els in
    match dst with
      | None ->
          unknown "log destination"
      | Some v ->
          log#set_max_level level;
          log#set_destination v
    
let fill_acl attrs =
  let v = need (List.assoc "jid") attrs "acl MUST have jid attribute" in
  let jid = need JID.of_string v "acl/jid MUST be user@server value" in
  let classname = need (List.assoc "class") attrs
    "acl MUST have class attribute" in
    Acl.acls := (jid, classname) :: !Acl.acls
      
let get_plugins els =
  let res =
    List.fold_left (fun acc -> function
                      | Xmlelement ("plugin", attrs, els) ->
                          let name = need (List.assoc "name") attrs
                            "plugin MUST have name attribute" in
                          let path = catch (List.assoc "path") attrs in
                          let opts =
                            List.fold_left (fun acc -> function
                                              | Xmlelement (name, args, _) ->
                                                  (name, args) :: acc
                                              | Xmlcdata _ -> acc
                                           ) [] els in
                            (name, path, List.rev opts) :: acc
                      | Xmlelement _ ->
                          acc
                      | Xmlcdata _ ->
                          acc
                   ) [] els in
    List.rev res
      
let read_config = function
  | Xmlelement ("sulci", _, els) ->
      let accounts, plugins =
        List.fold_left
          (fun (accounts, plugins) -> function
             | Xmlelement (name, attrs, els) -> (
                 match name with
                   | "account" ->
                       let value = need (List.assoc "jid") attrs
                         "account/jid attribute MUST present" in
                       let jid = need JID.of_string value
                         "account/jid MUST be user@server value" in
                       let account = parse_account jid els in
                         (account :: accounts, plugins)
                   | "log" ->
                       setup_logger els;
                       (accounts, plugins)
                   | "lang" ->
                       Lang.dir := need (List.assoc "dir") attrs
                         "lang/dir MUST be defined";
                       Lang.deflang :=
                         (try List.assoc "default" attrs
                          with Not_found -> "ru");
                       (accounts, plugins)
                   | "acl" ->
                       fill_acl attrs;
                       (accounts, plugins)
                   | "plugins" ->
                       let plugins = get_plugins els in
                         (accounts, plugins)
                   | other ->
                       unknown other
               )
             | Xmlcdata _ ->
                 (accounts, plugins)
          ) ([], []) els in
        List.rev accounts, plugins
  | Xmlelement _
  | Xmlcdata _ ->
      unknown "Bad configuration file"

let version () =
  Printf.printf 
    "%s %s (c) 2004-2011, Anastasia Gornostaeva\n"
    Version.name
    Version.version;
  Pervasives.exit 0

let default_cfile () =
  let bfile = Filename.basename Sys.argv.(0) in
  let file =
    try Filename.chop_extension bfile with Invalid_argument _ -> bfile in
    file ^ ".conf"

let get_config () =
  let usage_msg = Filename.basename Sys.argv.(0) ^ " [options]" in
  let cfile = ref (default_cfile ()) in
  let daemon_mode = ref false in
  let daemon () = daemon_mode := true in
  let opts = align [
    "-c", Set_string cfile, "<file>  Path to the config file";
    "-d", Unit daemon, "Daemon mode";
    "-v", Unit version, " Show version";
  ] in
  let () = Arg.parse opts
    (fun unk -> Printf.eprintf "Unknown option %S\n" unk;
       usage opts usage_msg;
       Pervasives.exit 1)
    usage_msg
  in
    if not (Sys.file_exists !cfile) then (
      Printf.eprintf "Cannot find a configuration file: %s\n" !cfile;
      Pervasives.exit 127
    )
    else
      let f = open_in !cfile in
      let buf = Buffer.create 1024 in
      let rec read_file () =
        let line = try Some (input_line f) with End_of_file -> None in
          match line with
            | None -> close_in f; Buffer.contents buf
            | Some v -> Buffer.add_string buf v; read_file ()
      in
      let content = read_file () in
      let xml = parse_document content in
        !daemon_mode, read_config xml
