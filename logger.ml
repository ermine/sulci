open Unix
open Config

let fout = open_out 
   (try 
       Xml.get_cdata config ~path:["lifelog"]
    with Not_found -> 
       "sulci_life.log")

let out str =
   let time = Strftime.strftime "%D %T" ~tm:(localtime (gettimeofday ())) in
      output_string fout (time ^ " " ^ str);
      output_string fout "\n";
      flush fout

let print_exn file ?xml exn =
   out ("Catched exception in file " ^ file ^ ": " ^
           (Printexc.to_string exn) ^ "\n" ^
           (match xml with
               | None -> ""
               | Some x ->
                    (Xml.element_to_string x)))

