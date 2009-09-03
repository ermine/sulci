(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open XMPP
open Jid
open Common
open Hooks

let prefix = ref ""

type command = {
  callback : xmpp -> env -> message_type option -> jid -> string -> unit;
  access : string
}

let commands : (string, command) Hashtbl.t = Hashtbl.create 10

let add_command (command:string) proc access =
  Hashtbl.replace commands command {callback = proc; access = access}

let parse_command_params params =
  let (key, name, access) =
    List.fold_left (fun (key, name, access) (p, v) ->
                      match p with
                        | "key" ->
                            if v = "" then (
                              log#notice "command/key is empty";
                              (key, name, access)
                            )
                            else
                              (v, name, access)
                        | "name" ->
                            if v = "" then (
                              log#notice "command/name is empty";
                              (key, name, access)
                            )
                            else
                              (key, v, access)
                        | "access" ->
                            if v = "" then (
                              log#notice "command/access is empty";
                              (key, name, access)
                            )
                            else
                              (key, name, v)
                        | other ->
                            log#notice "Unknown command parameter: %s" other;
                            (key, name, access)
                   ) ("", "", "") params in
    (key, name, access)

let add_commands cmds opts =
  let cmd_opts =
    List.fold_left (fun acc -> function
                      | "command", params ->
                          parse_command_params params :: acc
                      | _ -> acc
                   ) [] opts 
  in  
    if List.length cmds = 1 then
      let key, proc = List.hd cmds in
      let (_, name, access) =
        List.find (fun (key', _, _) -> key' = key) cmd_opts in
        add_command name proc access
    else
      List.iter (fun (key, proc) ->
                   try
                     let (_, name, access) =
                       List.find (fun (key', _, _) -> key = key') cmd_opts in
                       add_command name proc access
                   with Not_found ->
                     add_command key proc ""
                ) cmds

let do_command xmpp env kind jid_from text =
  if !prefix = "" || is_prefix !prefix text then
    let start = String.length !prefix in
    let command =
      try let sp = String.index_from text start ' ' in
        String.sub text start (sp - start)
      with Not_found ->
        string_after text start
    in
    let proc = try Some (Hashtbl.find commands command) with Not_found -> None in
      match proc with
        | None -> ()
        | Some c ->
            if env.env_check_access jid_from c.access then
              let params = try
                string_after text (String.index text ' ') with _ -> "" in
                try c.callback xmpp env kind jid_from (trim params) with exn -> 
                  log#error "[executing command %s] %s" command
                    (Printexc.to_string exn);
                  log#debug "%s" (Printexc.get_backtrace ())
            else
              () (* todo *)
  else
    ()
        
let process_message xmpp env stanza hooks =
  match stanza.jid_from, stanza.content.subject, stanza.content.body with
    | Some from, None, Some text ->
        let flag =
          if text <> "" then (
            do_command xmpp env stanza.kind from text;
            false
          )
          else
            true
        in
          if flag then
            do_hook xmpp env stanza hooks
    | _ ->
        do_hook xmpp env stanza hooks

let list_commands xmpp env kind jid_from text =
  let clist = Hashtbl.fold (fun key _ acc -> key :: acc) commands [] in
  let rsp =
    if clist = [] then
      "no commands yet"
    else
      String.concat " " (List.rev clist)
  in
    env.env_message xmpp kind jid_from rsp

let help xmpp env kind jid_from text =
  env.env_message xmpp kind jid_from "no help yet"

let plugin opts =
  add_commands [("help", help); ("commands", list_commands)] opts;
  let pref =
    try let v = List.assoc "prefix" opts in List.assoc "value" v
    with Not_found -> "" in
    prefix := pref;
    add_message_hook 70 "commands" process_message

let _ =
  add_plugin "commands" plugin
