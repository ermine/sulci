(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Hooks

let timerQ = Scheduler.create ()
let _ = Scheduler.run timerQ

let plugin opts =
  ()

let _ =
  add_plugin "scheduler" plugin
