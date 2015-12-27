(*
 * (c) 2007-2008 Anastasia Gornostaeva
 *)

module type EltOrdered =
sig
  type t
  val compare: t -> t -> int
end

module HeapQueue (Elt : EltOrdered) =
struct
  exception Empty

  type 'a t = {
	  mutable n: int;
	  a: Elt.t option array;
  }

  let is_empty q = q.n = 0

  let create () =
	  { n = 0; a = Array.create 8192 None; }
      
  let swap a i j =
	  let tmp = a.(i) in
	    a.(i) <- a.(j);
	    a.(j) <- tmp

  let lt q e1 e2 =
	  match q.a.(e1), q.a.(e2) with
	    | Some x, Some y -> Elt.compare x y = -1
	    | Some _, None -> true
	    | None, Some _ -> false
	    | None, None -> false
          
  let rec sift_down q i =
	  let child_l = i*2 + 1 in
	  let child_r = i*2 + 2 in
	    if child_r < q.n && lt q child_r child_l then (
	      if lt q child_r i then (
		      swap q.a i child_r;
		      sift_down q child_r
	      ))
	    else if child_l < q.n && lt q child_l i then (
	      swap q.a i child_l;
	      sift_down q child_l
	    )
	      
  let rec sift_up q i =
	  let parent = (i - 1) / 2 in
	    if i > 0 && lt q i parent then (
	      swap q.a i parent;
	      sift_up q parent
	    )
	      
  let add q x =
	  let n = q.n in
	    q.a.(n) <- Some x;
	    q.n <- n+1;
	    sift_up q n
	      
  let remove q i =
	  if i < 0 || i >= q.n then
	    raise (Invalid_argument "index out of range")
	  else
	    let n = q.n in
        q.n <- n - 1;
        if i + 1 < n then (
		      swap q.a i (n - 1);
		      sift_down q i
	      )
		      
  let peek q =
	  if q.n = 0 then
	    raise Empty
	  else
	    let r = q.a.(0) in
	      match r with
		      | Some x -> x
		      | None -> raise (Invalid_argument "malformed queue")
              
  let take q =
	  if q.n = 0 then 
	    raise Empty
	  else
	    let r = q.a.(0) in
        remove q 0;
	      match r with
		      | Some x -> x
		      | None -> raise (Invalid_argument "malformed queue")
              
end
