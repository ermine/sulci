(*
 * date file:line: string
 *)

open Logger

let _ =
(*
   let cmd = "/usr/local/sbin/cronolog -S /home/ermine/devel/log/yo-error.log /home/ermine/devel/log/yo-error-%Y-%m-%d.log" in
   let log = new logger ~destination:(new log_piped cmd) () in
*)

   let log = new logger ~max_level:"debug" 
      (* ~destination:(new syslog "local0") () in *)
      ~destination:new log_stderr () in
      log#debug LOG_LOC "debuging";
      log#debug "%s: debug string %s" LOG_FILE "abc";

      let a = 2 +3 in
      let () = log#printf LOG_DEBUG "abc %d" a in
	 try
	    int_of_string "a"
	 with exn ->
	    log#alert "%s" (Printexc.to_string exn);
	    raise exn
