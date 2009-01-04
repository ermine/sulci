(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
*)

exception MathNumberTooBig
exception MathNegNumber
exception MathCannotFloatFact

let fact x =
  if x > 174.0 then raise MathNumberTooBig
  else if x -. floor x <> 0.0 then raise MathCannotFloatFact
  else if x < 0.0 then raise MathNegNumber
  else
    let rec f y acc =
      if y = 0.0 then acc
      else f (y -. 1.) (y *. acc)
    in
      f x 1.
        
let fib n =
  let phi = (sqrt(5.) +. 1.) /. 2. in
  let ihp = 1. -. phi in
    (phi ** n -. ihp ** n) /. sqrt(5.)
      
