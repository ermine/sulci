(*
 * (c) 2009 Anastasia Gornostaeva
 * 
 * http://en.wikipedia.org/wiki/Treap 
 * The implementation based on Alexey Scshepin's code
 *)

module type OrderedType =
sig
  type t
  val compare: t -> t -> int
end

exception Empty

module Set (Ord: OrderedType) =
struct
  type elt = Ord.t
  type t =
    | Node of int * elt * int * t * t
    | Leaf
        
  let height = function
    | Node (s, _elt, _priority, _left, _right) -> s
    | Leaf -> 0
        
  let empty = Leaf

  let is_empty t = t = Leaf
    
  let create elt priority left right =
    Node (height left + height right + 1, elt, priority, left, right)
      
  let balance = function
    | Node (_s, elt, priority,
            (Node (_ls, lelt, lpriority, lleft, lright) as left),
            (Node (_rs, relt, rpriority, rleft, rright) as right)) as t ->
        if lpriority > priority && lpriority >= rpriority then
          create lelt lpriority
            lleft
            (create elt priority lright right)
        else if rpriority > priority then
          create relt rpriority
            (create elt priority left rleft)
            rright
        else
          t
    | Node (_s, elt, priority,
            Node (_ls, lelt, lpriority, lleft, lright),
            (Leaf as right)) as t ->
        if lpriority > priority then
          create lelt lpriority
            lleft
            (create elt priority lright right)
        else
          t
    | Node (_s, elt, priority,
            (Leaf as left),
            Node (_rs, relt, rpriority, rleft, rright)) as t ->
        if rpriority > priority then
          create relt rpriority
            (create elt priority left rleft)
            rright
        else
          t
    | Node (_, _, _, Leaf, Leaf)
    | Leaf as t ->
        t
        
  let rec add t new_elt new_priority =
    match t with
      | Node (size, elt, priority, left, right) ->
          let c = Ord.compare new_elt elt in
            if c = 0 then
              if new_priority <> priority then
                balance (Node (size, elt, new_priority, left, right))
              else
                t
            else if c < 0 then
              let left = add left new_elt new_priority in
                balance (create elt priority left right)
            else
              let right = add right new_elt new_priority in
                balance (create elt priority left right)
      | Leaf ->
          Node (1, new_elt, new_priority, Leaf, Leaf)

  let rec delete_root t =
    match t with
      | Node (_s, elt, priority,
              (Node (_ls, lelt, lpriority, lleft, lright) as left),
              (Node (_rs, relt, rpriority, rleft, rright) as right)) ->
          if lpriority > rpriority then
            create lelt lpriority
              lleft
              (delete_root (create elt priority lright right))
          else
            create relt rpriority
              (delete_root (create elt priority left rleft))
              rright
      | Node (_s, _elt, _priority, left, Leaf) ->
          left
      | Node (_s, _elt, _priority, Leaf, right) ->
          right
      | Leaf ->
          Leaf
            
  let rec delete t elt =
    match t with
      | Node (_s, elt', priority, left, right) ->
          let c = Ord.compare elt elt' in
            if c = 0 then
              delete_root t
            else if c < 0 then
              let left = delete left elt in
                create elt' priority left right
            else
              let right = delete right elt in
                create elt' priority left right
      | Leaf ->
          t
            
  let rec find t elt =
    match t with
      | Node (_size, elt', priority, left, right) ->
          let c = Ord.compare elt elt' in
            if c = 0 then
              priority
            else if c < 0 then
              find left elt
            else
              find right elt
      | Leaf ->
          raise Not_found

  let rec fold f acc = function
    |Leaf -> acc
    | Node (_s, elt, _priority, left, right) ->
        fold f (f (fold f acc left) elt) right
       
  let keys t =
    let rec aux_keys = function
      | Leaf -> []
      | Node (_s, elt, _priority, left, right) ->
          aux_keys left @ elt :: aux_keys right
    in
      aux_keys t

  let elements t =
    let rec aux_elements = function
      | Leaf -> []
      | Node (_s, elt, priority, left, right) ->
          aux_elements left @ (elt, priority) :: aux_elements right
    in
      aux_elements t

  let get_root = function
    | Leaf -> raise Empty
    | Node (_s, elt, priority, _left, _right) ->
        (elt, priority)

end
  
module Map  (Ord: OrderedType) =
struct
  type key = Ord.t
  type 'a t =
    | Node of int * key * 'a * int * 'a t * 'a t
    | Leaf
        
  let height = function
    | Node (s, _key, _value, _priority, _left, _right) -> s
    | Leaf -> 0
        
  let empty = Leaf
    
  let is_empty t = t = Leaf

  let create key value priority left right =
    Node (height left + height right + 1, key, value, priority, left, right)
      
  let balance = function
    | Node (_s, elt, value, priority,
            (Node (_ls, lelt, lvalue, lpriority, lleft, lright) as left),
            (Node (_rs, relt, rvalue, rpriority, rleft, rright) as right))
        as t ->
        if lpriority > priority && lpriority >= rpriority then
          create lelt lvalue lpriority
            lleft
            (create elt value priority lright right)
        else if rpriority > priority then
          create relt rvalue rpriority
            (create elt value priority left rleft)
            rright
        else
          t
    | Node (_s, elt, value, priority,
            Node (_ls, lelt, lvalue, lpriority, lleft, lright),
            (Leaf as right)) as t ->
        if lpriority > priority then
          create lelt lvalue lpriority
            lleft
            (create elt value priority lright right)
        else
          t
    | Node (_s, elt, value, priority,
            (Leaf as left),
            Node (_rs, relt, rvalue, rpriority, rleft, rright)) as t ->
        if rpriority > priority then
          create relt rvalue rpriority
            (create elt value priority left rleft)
            rright
        else
          t
    | Node (_, _, _, _, Leaf, Leaf)
    | Leaf as t ->
        t
        
  let rec add t new_elt new_value new_priority =
    match t with
      | Node (size, elt, value, priority, left, right) ->
          let c = Ord.compare new_elt elt in
            if c = 0 then
              if new_priority <> priority then
                balance (Node (size, elt, new_value, new_priority, left, right))
              else
                t
            else if c < 0 then
              let left = add left new_elt new_value new_priority in
                balance (create elt value priority left right)
            else
              let right = add right new_elt new_value new_priority in
                balance (create elt value priority left right)
      | Leaf ->
          Node (1, new_elt, new_value, new_priority, Leaf, Leaf)

  let rec delete_root t =
    match t with
      | Node (_s, elt, value, priority,
              (Node (_ls, lelt, lvalue, lpriority, lleft, lright) as left),
              (Node (_rs, relt, rvalue, rpriority, rleft, rright) as right)) ->
          if lpriority > rpriority then
            create lelt lvalue lpriority
              lleft
              (delete_root (create elt value priority lright right))
          else
            create relt rvalue rpriority
              (delete_root (create elt value priority left rleft))
              rright
      | Node (_s, _elt, _value, _priority, left, Leaf) ->
          left
      | Node (_s, _elt, _value, _priority, Leaf, right) ->
          right
      | Leaf ->
          Leaf
            
  let rec delete t elt =
    match t with
      | Node (_s, elt', value, priority, left, right) ->
          let c = Ord.compare elt elt' in
            if c = 0 then
              delete_root t
            else if c < 0 then
              let left = delete left elt in
                create elt' value priority left right
            else
              let right = delete right elt in
                create elt' value priority left right
      | Leaf ->
          t
            
  let rec find t elt =
    match t with
      | Node (_size, elt', value, priority, left, right) ->
          let c = Ord.compare elt elt' in
            if c = 0 then
              (value, priority)
            else if c < 0 then
              find left elt
            else
              find right elt
      | Leaf ->
          raise Not_found

  let rec fold f acc = function
    |Leaf -> acc
    | Node (_s, elt, value, _priority, left, right) ->
        fold f (f (fold f acc left) elt value) right

  let keys t =
    let rec aux_keys = function
      | Leaf -> []
      | Node (_s, elt, _value, _priority, left, right) ->
          aux_keys left @ elt :: aux_keys right
    in
      aux_keys t

  let elements t =
    let rec aux_elements = function
      | Leaf -> []
      | Node (_s, elt, value, priority, left, right) ->
          aux_elements left @ (elt, value, priority) :: aux_elements right
    in
      aux_elements t

  let get_root = function
    | Leaf -> raise Empty
    | Node (_s, elt, value, priority, _left, _right) ->
        (elt, value, priority)
end
