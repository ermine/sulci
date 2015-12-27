(*
 * (c) 2009 Anastasia Gornostaeva
 * 
 * http://en.wikipedia.org/wiki/Treap 
 * The implementation based on Alexey Scshepin's code
 *)

module type OrderedType = sig type t val compare : t -> t -> int end

exception Empty

module Set :
  functor (Ord : OrderedType) ->
    sig
      type elt = Ord.t
      type t = Node of int * elt * int * t * t | Leaf
      val height : t -> int
      val empty : t
      val is_empty : t -> bool
      val create : elt -> int -> t -> t -> t
      val balance : t -> t
      val add : t -> elt -> int -> t
      val delete_root : t -> t
      val delete : t -> Ord.t -> t
      val find : t -> Ord.t -> int
      val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
      val keys : t -> elt list
      val elements : t -> (elt * int) list
      val get_root : t -> elt * int
    end

module Map :
  functor (Ord : OrderedType) ->
    sig
      type key = Ord.t
      type 'a t = Node of int * key * 'a * int * 'a t * 'a t | Leaf
      val height : 'a t -> int
      val empty : 'a t
      val is_empty : 'a t -> bool
      val create : key -> 'a -> int -> 'a t -> 'a t -> 'a t
      val balance : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> int -> 'a t
      val delete_root : 'a t -> 'a t
      val delete : 'a t -> Ord.t -> 'a t
      val find : 'a t -> Ord.t -> 'a * int
      val fold : ('a -> key -> 'b -> 'a) -> 'a -> 'b t -> 'a
      val keys : 'a t -> key list
      val elements : 'a t -> (key * 'a * int) list
      val get_root : 'a t -> key * 'a * int
    end
