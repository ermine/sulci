module Elt =
struct
  type t = int
  let compare = compare
end

module Ints = Treap.Set(Elt)

let _ =
  Random.self_init ();
  let t =
    List.fold_left (fun t elt ->
                      (try
                         let e, p = Ints.get_root t in
                           Printf.printf "Root %d %d\n" e p
                       with _ -> ());
                      let r = Random.int 100 in
                        Printf.printf "add %d %d\n" elt r;
                        Ints.add t elt r
                   )
      Ints.empty
      [1;2;3;4;5;6;7;8;9;10] in

    Printf.printf "height %d\n" (Ints.height t);
    
    let k = Ints.elements t in
      List.iter (fun (i,p) ->
                   print_int i;
                   print_string " ";
                   print_int p;
                   print_newline ()) k;
      
      let _ =
        List.fold_left (fun t elt ->
                        (try
                           let e, p = Ints.get_root t in
                             Printf.printf "Root %d %d\n" e p
                         with _ -> ());
                          Printf.printf "delete %d\n" elt;
                          Ints.delete t elt
                     ) t [1;2;3;4;5;6;7;8;9;10] in
      ()

    
