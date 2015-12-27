(*
 * (c) 2009-2011 Anastasia Gornostaeva
 *)

(*
 * Cmd  Effect
 * ---  ------
 * +    Increases element under pointer
 * -    Decrases element under pointer
 * >    Increases pointer
 * <    Decreases pointer
 * [    Starts loop, flag under pointer
 * ]    Indicates end of loop
 * .    Outputs ASCII code under pointer
 * ,    Reads char and stores ASCII under ptr
 *)

exception Error of string

let execute ?(array_size=30000) ?(loops_limit=10000000)
    str user_putchar user_getchar =
  let a = Array.make array_size 0 in
  let stub = (fun _iters _pointer _i -> ()) in
  let code = Array.make (String.length str) stub in
  let rec aux_loop iters pointer i =
    if i < Array.length code then
      code.(i) iters pointer i
    else
      ()
  and plus iters pointer i =
    a.(pointer) <- succ a.(pointer);
    aux_loop iters pointer (succ i)
  and minus iters pointer i=
    a.(pointer) <- pred a.(pointer);
    aux_loop iters pointer (succ i)
  and shift_left iters pointer i=
    aux_loop iters (pred pointer) (succ i)
  and shift_right iters pointer i=
    aux_loop iters (succ pointer) (succ i)
  and putchar iters pointer i =
    user_putchar a.(pointer);
    aux_loop iters pointer (succ i)
  and getchar iters pointer i =
    a.(pointer) <- user_getchar ();
    aux_loop iters pointer i
  and start_loop jmp iters pointer i=
    if a.(pointer) <> 0 then
      if iters < loops_limit then
        aux_loop (succ iters) pointer (succ i)
      else
        raise (Error "Iteration limit exceed")
    else
      aux_loop iters pointer jmp
  and end_loop jmp iters pointer _i =
    aux_loop iters pointer jmp
  and ign iters pointer i =
    aux_loop iters pointer (succ i)
  in
  let rec parse ps i =
    if i < String.length str then
      match str.[i] with
        | '+' -> code.(i) <- plus; parse ps (succ i)
        | '-' -> code.(i) <- minus; parse ps (succ i)
        | '<' -> code.(i) <- shift_left; parse ps (succ i)
        | '>' -> code.(i) <- shift_right; parse ps (succ i)
        | '.' -> code.(i) <- putchar; parse ps (succ i)
        | ',' -> code.(i) <- getchar; parse ps (succ i)
        | '[' -> parse (i::ps) (succ i)
        | ']' ->
            if ps = [] then
              raise (Error "Unmatched ]")
            else
              let j = List.hd ps in
                code.(j) <- start_loop (succ i);
                code.(i) <- end_loop j;
                parse (List.tl ps) (succ i)
        | _ -> code.(i) <- ign; parse ps (succ i)
    else
      if ps <> [] then
        raise (Error "Unmatched [")
  in
    parse []  0;
    aux_loop 0 0 0
