(*
 * (c) 2005-2008 Anastasia Gornostaeva
 *)

type elt = {
  mutable time : float;
  repeat : unit -> float;
  callback : unit -> unit;
  mutable cancelled : bool;
}

module TimerOrdered : sig type t = elt val compare : elt -> elt -> int end

module TimerQueue :
  sig
    exception Empty
    type 'a t =
      'a Heapqueue.HeapQueue(TimerOrdered).t = {
      mutable n : int;
      a : TimerOrdered.t option array;
    }
    val is_empty : 'a t -> bool
    val create : unit -> 'a t
    val swap : 'a array -> int -> int -> unit
    val lt : 'a t -> int -> int -> bool
    val sift_down : 'a t -> int -> unit
    val sift_up : 'a t -> int -> unit
    val add : 'a t -> TimerOrdered.t -> unit
    val remove : 'a t -> int -> unit
    val peek : 'a t -> TimerOrdered.t
    val take : 'a t -> TimerOrdered.t
  end

type t = {
  reader : Unix.file_descr;
  writer : Unix.file_descr;
  queue : elt TimerQueue.t;
  mutex : Mutex.t;
  mutable input_wrote : bool;
}

val create : unit -> t

val add_task : t -> (unit -> unit) -> float -> (unit -> float) -> elt

val run : t -> Thread.t
