(*
 * (c) 2004-2010 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open XMPP
open JID
open Common
open Hooks
open Acl

type command = {
  callback : xmpp -> env -> message_type option -> jid -> string -> unit;
  access : string
}

type command_context = {
  mutable prefix : string;
  commands : (string, command) Hashtbl.t
}

let storage = Hashtbl.create 2

let get_context xmpp =
  Hashtbl.find storage xmpp.data.skey
    (* try .. with -> error *)

let add_command xmpp id (command:string) proc access =
  log#info "Registered a command id: %s name: %s access: %s" id command access;
  let ctx = get_context xmpp in
    Hashtbl.replace ctx.commands command {callback = proc; access = access}

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

let parse_opts cmds opts =
  let cmd_opts =
    List.fold_left (fun acc -> function
                      | "command", params ->
                          parse_command_params params :: acc
                      | _ -> acc
                   ) [] opts in
  let res =
    List.fold_left (fun acc (id, proc) ->
                      let id', name, access =
                        try List.find (fun (id', _, _) -> id' = id) cmd_opts
                        with Not_found -> (id, "", "") in
                      let name = if name = "" then id else name in
                        (id, name, access, proc) :: acc
                   ) [] cmds
  in List.rev res
      
let add_commands xmpp cmds opts =
  let data = parse_opts cmds opts in
    List.iter (fun (id, name, access, proc) ->
                 add_command xmpp id name proc access
              ) data

let do_command ctx xmpp env kind jid_from text =
  if text <> "" && (ctx.prefix = "" || is_prefix ctx.prefix text) then
    let start = String.length ctx.prefix in
    let command =
      try let sp = String.index_from text start ' ' in
        String.sub text start (sp - start)
      with Not_found ->
        string_after text start
    in
    let proc =
      try Some (Hashtbl.find ctx.commands command)
      with Not_found -> None in
      match proc with
        | None -> true
        | Some c ->
            if check_access jid_from c.access then
              let params =
                try string_after text (String.index text ' ') with _ -> ""
              in
                try c.callback xmpp env kind jid_from (trim params) with exn -> 
                  log#error "[executing command %s] %s" command
                    (Printexc.to_string exn);
                  log#debug "%s" (Printexc.get_backtrace ())
            else
              env.env_message xmpp kind jid_from "no access";
            false
  else
    true
        
let process_message xmpp env stanza hooks =
  match stanza.jid_from, stanza.content.subject, stanza.content.body with
    | Some from, None, Some text ->
        let ctx = get_context xmpp in
          if do_command ctx xmpp env stanza.content.message_type from text then
            do_hook xmpp env stanza hooks
    | _ ->
        do_hook xmpp env stanza hooks

let list_commands xmpp env kind jid_from text =
  let ctx = get_context xmpp in
  let clist = Hashtbl.fold (fun id _ acc -> id :: acc) ctx.commands [] in
  let rsp =
    if clist = [] then
      "no commands yet"
    else
      String.concat " " (List.fast_sort compare clist)
  in
    env.env_message xmpp kind jid_from rsp

let help xmpp env kind jid_from text =
  let _ctx = get_context xmpp in
    env.env_message xmpp kind jid_from "no help yet"

let plugin opts =
  let prefix =
    try let v = List.assoc "prefix" opts in List.assoc "value" v
    with Not_found -> ""
  in
  let spec = parse_opts [("help", help); ("commands", list_commands)] opts in
    add_for_token
      (fun _opts xmpp ->
         let ctx =
           {prefix = prefix;
            commands = Hashtbl.create 10};
         in
           Hashtbl.add storage xmpp.data.skey ctx;
           add_message_hook xmpp 70 "commands" process_message;
           List.iter (fun (id, name, access, proc) ->
                        Hashtbl.add ctx.commands name
                          {callback = proc; access = access}
                     ) spec;
      )
      
let () =
  Plugin.add_plugin "commands" plugin
