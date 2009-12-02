(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Jid
open Hooks
  
let acls : ((jid * string) list) ref = ref []

let check_access jid classname =
  if classname = "" then
    true
  else
    List.exists
      (fun (jid', name) ->
         (name = classname &&
             jid'.lnode = jid.lnode && jid'.ldomain = jid.ldomain)
        ) !acls

  
