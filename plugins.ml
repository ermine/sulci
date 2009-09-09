(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Hooks

let loadfile name path opts =
  let loaded =
    if List.mem_assoc name global.plugins then (
      log#info "Plugin %s is already loaded" name;
      true
    )
    else
      let file = path ^ (if Dynlink.is_native then ".cmxs" else ".cmo") in
        try
          Dynlink.loadfile file;
          log#info "Plugin %s (%s) is successfully loaded" name file;
          true
        with Dynlink.Error (Dynlink.File_not_found str) ->
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
  in
    if loaded then
      try
        let plugin = List.assoc name global.plugins in
          plugin opts
      with exn ->
        log#error "Plugin %s failed: %s" name (Printexc.to_string exn)

let load_plugins plugins =
  List.iter (fun (name, path, opts) ->
               log#info "Loading plugin %s (%s)" name path;
               loadfile name path opts
            ) plugins
