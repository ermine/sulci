(*
 * 2005-2010 (c) Anastasia Gornostaeva
 *)

open Unix

type level_t =
  | LOG_DEBUG
  | LOG_INFO
  | LOG_NOTICE
  | LOG_WARNING
  | LOG_ERR
  | LOG_CRIT
  | LOG_ALERT
  | LOG_EMERG

let level_of_string = function
  | "debug" -> LOG_DEBUG
  | "info" -> LOG_INFO
  | "notice" -> LOG_NOTICE
  | "warning" -> LOG_WARNING
  | "warn" -> LOG_WARNING
  | "err" -> LOG_ERR
  | "error" -> LOG_ERR
  | "crit" -> LOG_CRIT
  | "alert" -> LOG_ALERT
  | "emerg" -> LOG_EMERG
  | x ->
	    failwith ("Unknown log level: " ^ x)

let int_of_level = function
  | LOG_EMERG   -> 0
  | LOG_ALERT   -> 1
  | LOG_CRIT    -> 2
  | LOG_ERR     -> 3
  | LOG_WARNING -> 4
  | LOG_NOTICE  -> 5
  | LOG_INFO    -> 6
  | LOG_DEBUG   -> 7

type facility = 
  | LOG_KERN       (* kernel messages *)
  | LOG_USER       (* random user-level messages *)
  | LOG_MAIL       (* mail system *)
  | LOG_DAEMON     (* system daemons *)
  | LOG_AUTH       (* authorization messages *)
  | LOG_SYSLOG     (* messages generated internally by syslogd *)
  | LOG_LPR        (* line printer subsystem *)
  | LOG_NEWS       (* network news subsystem *)
  | LOG_UUCP       (* UUCP subsystem *)
  | LOG_CRON       (* clock daemon *)
  | LOG_AUTHPRIV   (* authorization messages (private) *)
  | LOG_FTP        (* ftp daemon *)
  | LOG_NTP        (* NTP subsystem *)
  | LOG_SECURITY   (* security subsystems (firewalling, etc.) *)
  | LOG_CONSOLE    (* /dev/console output *)
  | LOG_LOCAL0     (* reserved for local use *)
  | LOG_LOCAL1     (* reserved for local use *)
  | LOG_LOCAL2     (* reserved for local use *)
  | LOG_LOCAL3     (* reserved for local use *)
  | LOG_LOCAL4     (* reserved for local use *)
  | LOG_LOCAL5     (* reserved for local use *)
  | LOG_LOCAL6     (* reserved for local use *)
  | LOG_LOCAL7     (* reserved for local use *)

let int_of_facility = function
  | LOG_KERN ->      0 lsl 3   (* 0<<3 *)
  | LOG_USER ->      1 lsl 3   (* 1<<3 *)
  | LOG_MAIL ->      2 lsl 3   (* 2<<3 *)
  | LOG_DAEMON ->    3 lsl 3   (* 3<<3 *)
  | LOG_AUTH ->      4 lsl 3   (* 4<<3 *)
  | LOG_SYSLOG ->    5 lsl 3   (* 5<<3 *)
  | LOG_LPR ->       6 lsl 3   (* 6<<3 *)
  | LOG_NEWS ->      7 lsl 3   (* 7<<3 *)
  | LOG_UUCP ->      8 lsl 3   (* 8<<3 *)
  | LOG_CRON ->      9 lsl 3   (* 9<<3 *)
  | LOG_AUTHPRIV ->  10 lsl 3  (* 10<<3 *)
  | LOG_FTP ->       11 lsl 3  (* 11<<3 *)
  | LOG_NTP ->       12 lsl 3  (* 12<<3 *)
  | LOG_SECURITY ->  13 lsl 3  (* 13<<3 *)
  | LOG_CONSOLE ->   14 lsl 3  (* 14<<3 *)
  | LOG_LOCAL0 ->    16 lsl 3  (* 16<<3 *)
  | LOG_LOCAL1 ->    17 lsl 3  (* 17<<3 *)
  | LOG_LOCAL2 ->    18 lsl 3  (* 18<<3 *)
  | LOG_LOCAL3 ->    19 lsl 3  (* 19<<3 *)
  | LOG_LOCAL4 ->    20 lsl 3  (* 20<<3 *)
  | LOG_LOCAL5 ->    21 lsl 3  (* 21<<3 *)
  | LOG_LOCAL6 ->    22 lsl 3  (* 22<<3 *)
  | LOG_LOCAL7 ->    23 lsl 3  (* 23<<3 *)

let facility_of_string = function
  | "kern" -> LOG_KERN
  | "user" -> LOG_USER
  | "mail" -> LOG_MAIL
  | "daemon" -> LOG_DAEMON
  | "auth" -> LOG_AUTH
  | "syslog" -> LOG_SYSLOG
  | "lpr" -> LOG_LPR
  | "news" -> LOG_NEWS
  | "uucp" -> LOG_UUCP
  | "cron" -> LOG_CRON
  | "authpriv" -> LOG_AUTHPRIV
  | "ftp" -> LOG_FTP
  | "ntp" -> LOG_NTP
  | "security" -> LOG_SECURITY
  | "console" -> LOG_CONSOLE
  | "local0" -> LOG_LOCAL0
  | "local1" -> LOG_LOCAL1
  | "local2" -> LOG_LOCAL2
  | "local3" -> LOG_LOCAL3
   | "local4" -> LOG_LOCAL4
   | "local5" -> LOG_LOCAL5
   | "local6" -> LOG_LOCAL6
   | "local7" -> LOG_LOCAL7
   | x -> failwith ("Unknown syslog facility: " ^ x)


class type log_destination =
object
  method reopen: unit
  method close: unit
  method write: level_t -> string -> unit
end

class log_stderr:log_destination =
object
  method close = ()
  method reopen = ()
  method write _level str =
    output_string Pervasives.stderr str;
    output_string Pervasives.stderr "\n";
    flush Pervasives.stderr
end

let time_format_fn () =
  let tm = localtime (gettimeofday ()) in
    Printf.sprintf "%02d/%02d/%d %02d:%02d:%02d" 
	    tm.tm_mday (tm.tm_mon+1) (tm.tm_year+1900)
	    tm.tm_hour tm.tm_min tm.tm_sec

class logfile ?time_format ?truncate filename : log_destination =
  let time_format_fn = 
    match time_format with
	    | None -> time_format_fn
	    | Some f -> f 
  in
  let open_file () =
    match truncate with
	    | Some true ->
	        open_out filename
	    | _ ->
	        if Sys.file_exists filename then
		        open_out filename
	        else
		        open_out_gen [Open_wronly; Open_append; Open_creat] 
		          0o664 filename
  in
object
  val mutable ch = open_file ()
    
  method reopen =
    close_out ch;
    ch <- open_file ()
      
  method close =
    close_out ch

  method write _level str =
    output_string ch (time_format_fn ());
    output_string ch " ";
    output_string ch str;
    output_string ch "\n";
    flush ch
end

class syslog facility_s : log_destination =
  let path = 
    let path = "/var/run/log" in
	    if Sys.file_exists path && (Unix.stat path).st_kind = S_SOCK then
	      path
	    else let path = "/dev/log" in
	      if Sys.file_exists path && (Unix.stat path).st_kind = S_SOCK then
	        path
	      else
	        failwith "Cannot find syslog socket file"
  in
  let sockaddr = ADDR_UNIX path in
  let fd = socket PF_UNIX SOCK_DGRAM 0 in
  let progname =
    let len = String.length Sys.executable_name in
	    try
	      let r = String.rindex Sys.executable_name '/' + 1 in
	        if r < len then
		        String.sub Sys.executable_name r (len-r)
	        else
		        raise Not_found
	    with Not_found ->
	      Sys.executable_name
  in
  let pid = Unix.getpid () in
  let facility_int = int_of_facility (facility_of_string facility_s) in
object
  val mutable fd = fd
    
  method reopen =
    close fd;
    fd <- socket PF_UNIX SOCK_DGRAM 0
      
  method close = 
    close fd
      
  method write level str =
    let msg = Printf.sprintf "<%d> %s[%d]: %s\n"
	    (facility_int + int_of_level level)
	    progname pid
	    str
    in
    let _lsent = sendto fd msg 0 (String.length msg) [] sockaddr in
	    ()
end		
    
class piped_log command : log_destination =
  let open_pipe () = open_process_out command in
object
  val mutable ch = open_pipe ()
    
  method reopen  =
    ignore (close_process_out ch);
    ch <- open_pipe ()
      
  method close = ignore (close_process_out ch)
    
  method write (_level:level_t) str =
    output_string ch str;
    output_string ch "\n";
    flush ch
end      
    
class logger ?max_level ?destination () =
  let dst = match destination with
    | None -> new log_stderr
    | Some d -> d
  in
  let max_level = 
    match max_level with
	    | None -> int_of_level LOG_INFO
	    | Some s -> int_of_level (level_of_string s)
  in
object (self)
  val mutable max_level = max_level
  val mutable dst = dst
    
  method set_max_level l = max_level <- int_of_level (level_of_string l)
    
  method get_destination () = dst

  method set_destination (d:log_destination) = dst <- d
    
  method printf: 'a. level_t -> ('a, unit, string, unit) format4 -> 'a =
    fun level fmt ->
	    let stub str =
	      if int_of_level level <= max_level then
	        dst#write level str
	    in
	      Printf.ksprintf stub fmt
          
  method debug : 'a. ('a, unit, string, unit) format4 -> 'a =
    self#printf LOG_DEBUG
      
  method info : 'a. ('a, unit, string, unit) format4 -> 'a =
    self#printf LOG_INFO
      
  method notice : 'a. ('a, unit, string, unit) format4 -> 'a =
    self#printf LOG_NOTICE
      
  method warning : 'a. ('a, unit, string, unit) format4 -> 'a =
    self#printf LOG_WARNING
      
  method error : 'a. ('a, unit, string, unit) format4 -> 'a =
    self#printf LOG_ERR
      
  method crit : 'a. ('a, unit, string, unit) format4 -> 'a =
    self#printf LOG_CRIT
      
  method alert : 'a. ('a, unit, string, unit) format4 -> 'a =
    self#printf LOG_ALERT
      
  method emerg : 'a. ('a, unit, string, unit) format4 -> 'a =
      self#printf LOG_EMERG
end
