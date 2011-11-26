(*
 * (c) 2004-2010 Anastasia Gornostaeva
 *)

open Unix
open Hooks

let timerQ = Scheduler.create ()
let _ = Scheduler.run timerQ

let get_next_time hour min () =
  let curr_tm = localtime (gettimeofday ()) in
  let noun, _ = mktime
    {curr_tm with 
       tm_sec = 0; tm_min = min; tm_hour = hour;
       tm_mday = (if curr_tm.tm_hour < hour then 
                    curr_tm.tm_mday else curr_tm.tm_mday + 1)} in
    
    noun

let plugin opts =
  ()

let () =
  Plugin.add_plugin "scheduler" plugin
