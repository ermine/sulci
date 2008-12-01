open Unix
open Config

let logfile =
  try List.assoc "report" Config.logger_options with Not_found -> "/dev/null"

let lout = ref Pervasives.stdout

let open_log () =
  lout := open_out_gen [Open_creat; Open_append] 0o644 logfile

let out str =
  let time = Strftime.strftime "%d/%m/%Y %T" 
    ~tm:(localtime (gettimeofday ())) in
    output_string !lout (time ^ " " ^ str ^ "\n");
    flush !lout
      
let print_exn file ?xml exn =
  out ("Caught exception in file " ^ file ^ ": " ^
         (Printexc.to_string exn) ^
         (match xml with
            | None -> ""
            | Some x ->
                "\n" ^ Xml.element_to_string x))

let _ =
  open_log ();
  Sys.set_signal Sys.sighup
    (Sys.Signal_handle (fun x -> 
                          let old_log = !lout in
                            open_log ();
                            close_out old_log))
