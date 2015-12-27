(*
 * (c) 2005-2009 Anastasia Gornostaeva
 *)

open Heapqueue

type elt = {
  mutable time: float;
  repeat: unit -> float;
  callback: unit -> unit;
  mutable cancelled: bool
}

module TimerOrdered =
struct 
  type t = elt
  let compare elt1 elt2 = compare elt1.time elt2.time
end

module TimerQueue = HeapQueue(TimerOrdered)

type t = {
  reader: Unix.file_descr;
  writer: Unix.file_descr;
  queue: elt TimerQueue.t;
  mutex: Mutex.t;
  mutable input_wrote: bool
}

let create () =
  let (f1, f2) = Unix.pipe () in
    { reader = f1;
    writer = f2;
	  queue = TimerQueue.create ();
	  mutex = Mutex.create ();
	  input_wrote = false
    }

let call timerQ =
  Mutex.lock timerQ.mutex;
  let r = TimerQueue.peek timerQ.queue in
    if r.cancelled then
	    TimerQueue.remove timerQ.queue 0
    else (
	    r.callback ();
	    let repeat = r.repeat () in
	      if repeat = 0.0 then
	        TimerQueue.remove timerQ.queue 0
	      else (
	        r.time <- repeat;
	        TimerQueue.sift_down timerQ.queue 0
	      )
    );
    Mutex.unlock timerQ.mutex

let add_task timerQ callback time repeat =
  let data= { time = time;
	repeat = repeat;
	callback = callback;
	cancelled = false
	} 
  in
    Mutex.lock timerQ.mutex;
    TimerQueue.add timerQ.queue data;
    if timerQ.input_wrote = false then (
	    timerQ.input_wrote <- true;
	    ignore (Unix.write timerQ.writer " " 0 1)
    );
    Mutex.unlock timerQ.mutex;
    data

let run timerQ =
  let rec scheduler () =
    let sleep = 
	    if TimerQueue.is_empty timerQ.queue then
	      10000000.
	    else
	      let task = TimerQueue.peek timerQ.queue in
	      let time = task.time -. Unix.gettimeofday () in
	        if time < 0. then 0.0 else time
    in
    let r, _, _ = Thread.select [timerQ.reader] [] [] sleep in
	    match r with
	      | [] ->
		        if not (TimerQueue.is_empty timerQ.queue) then (
		          let curr_time = Unix.gettimeofday () in
		          let task = TimerQueue.peek timerQ.queue in
		            if task.time <= curr_time then
			            call timerQ
		        );
		        scheduler ()
	      | _x :: _xs -> 
		        if timerQ.input_wrote then (
		          Mutex.lock timerQ.mutex;
		          let s = String.create 1 in
		          let _ = Unix.read timerQ.reader s 0 1 in
		            timerQ.input_wrote <- false;
		            Mutex.unlock timerQ.mutex
		        );
		        scheduler ()
  in
    Thread.create scheduler ()
