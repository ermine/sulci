(*                                                                          *)
(* (c) 2004, Anastasia Gornostaeva. <ermine@ermine.pp.ru>                   *)
(*                                                                          *)

open Jeps
open Hooks

let _ =
   Hooks.register_handle 
      (Xmlns ("jabber:iq:version", iq_version_reply "Sulci" Version.version))
      (* "jabber:iq:last", Iq.iq_last_reply *)
