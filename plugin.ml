(*
 * (c) 2004-2010 Anastasia Gornostaeva
 *)

open Hooks

exception Error of string

let pluginlist :
    ((string * ((string * (string * string) list) list -> unit)) list) ref
    = ref []

let add_plugin name proc =
  pluginlist := (name, proc) :: !pluginlist

let loadfile name path opts =
  let file = path ^ (if Dynlink.is_native then ".cmxs" else ".cmo") in
    try
      Dynlink.loadfile file;
      log#info "Plugin %s (%s) is successfully loaded" name file;
      true
    with
      | Dynlink.Error (Dynlink.File_not_found str) -> (
          if Dynlink.is_native then (
            log#error "Unable to load plugin %s (%s): %s"
              name path (Dynlink.error_message (Dynlink.File_not_found str));
            false
          ) else
            let file = path ^ ".cma" in
              try
                Dynlink.loadfile file;
                log#info "Plugin %s %s is successfully loaded" name file;
                true
              with Dynlink.Error err ->
                log#error "Unable to load plugin %s (%s): %s"
                  name path (Dynlink.error_message err);
                false
        )
      | Sys_error msg ->
          log#error "Unable to load plugin %s (%s): %s" name path msg;
          false
            
let load_plugins plugins =
  List.iter
    (fun (name, path, opts) ->
       log#info "Loading plugin %s" name;
       if List.mem_assoc name !pluginlist then (
         log#info "Plugin %s is already loaded" name;
         try
           let plugin = List.assoc name !pluginlist in
             plugin opts
         with exn ->
           log#error "Plugin %s failed: %s" name
             (Printexc.to_string exn)
       )
       else
         match path with
           | None -> log#warning "Plugin %s is not found" name
           | Some vpath ->
               if loadfile name vpath opts then
                 try
                   let plugin = List.assoc name !pluginlist in
                     plugin opts
                 with exn ->
                   log#error "Plugin %s failed: %s" name
                     (Printexc.to_string exn)
    ) plugins
