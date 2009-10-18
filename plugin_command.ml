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

let add_command id (command:string) proc access =
  log#info "Added a command id: %s name: %s access: %s" id command access;
  Hashtbl.replace commands command {callback = proc; access = access}

let parse_command_params params =
  let (id, name, access) =
    List.fold_left (fun (id, name, access) (p, v) ->
                      match p with
                        | "id" ->
                            if v = "" then (
                              log#notice "command/id is empty";
                              (id, name, access)
                            )
                            else
                              (v, name, access)
                        | "name" ->
                            if v = "" then (
                              log#notice "command/name is empty";
                              (id, name, access)
                            )
                            else
                              (id, v, access)
                        | "access" ->
                            if v = "" then (
                              log#notice "command/access is empty";
                              (id, name, access)
                            )
                            else
                              (id, name, v)
                        | other ->
                            log#notice "Unknown command parameter: %S" other;
                            (id, name, access)
                   ) ("", "", "") params in
    (id, name, access)

let add_commands cmds opts =
  let cmd_opts =
    List.fold_left (fun acc -> function
                      | "command", params ->
                          parse_command_params params :: acc
                      | _ -> acc
                   ) [] opts 
  in  
    if List.length cmds = 1 then
      let id, proc = List.hd cmds in
      let (_, name, access) =
        try List.find (fun (id', _, _) -> id' = id) cmd_opts
        with Not_found -> id, id, ""
      in
      let name = if name = "" then id else name in
        add_command id name proc access
    else
      List.iter (fun (id, proc) ->
                   try
                     let (_, name, access) =
                       List.find (fun (id', _, _) -> id = id') cmd_opts in
                     let name = if name = "" then id else name in
                       add_command id name proc access
                   with Not_found ->
                     add_command id id proc ""
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
            if check_access jid_from c.access then
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
  let clist = Hashtbl.fold (fun id _ acc -> id :: acc) commands [] in
  let rsp =
    if clist = [] then
      "no commands yet"
    else
      String.concat " " (List.fast_sort compare clist)
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
