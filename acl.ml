(*
 * (c) 2004-2010 Anastasia Gornostaeva
 *)

open JID
open Hooks
  
let acls : ((JID.t * string) list) ref = ref []

let check_access jid classname =
  if classname = "" then
    true
  else
    List.exists
      (fun (jid', name) ->
         (name = classname &&
             jid'.lnode = jid.lnode && jid'.ldomain = jid.ldomain)
        ) !acls

  
