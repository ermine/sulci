(*
 * 2005-2008 (c) Anastasia Gornostaeva
 *)

type level_t =
    LOG_DEBUG
  | LOG_INFO
  | LOG_NOTICE
  | LOG_WARNING
  | LOG_ERR
  | LOG_CRIT
  | LOG_ALERT
  | LOG_EMERG
val level_of_string : string -> level_t
val int_of_level : level_t -> int
type facility =
    LOG_KERN
  | LOG_USER
  | LOG_MAIL
  | LOG_DAEMON
  | LOG_AUTH
  | LOG_SYSLOG
  | LOG_LPR
  | LOG_NEWS
  | LOG_UUCP
  | LOG_CRON
  | LOG_AUTHPRIV
  | LOG_FTP
  | LOG_NTP
  | LOG_SECURITY
  | LOG_CONSOLE
  | LOG_LOCAL0
  | LOG_LOCAL1
  | LOG_LOCAL2
  | LOG_LOCAL3
  | LOG_LOCAL4
  | LOG_LOCAL5
  | LOG_LOCAL6
  | LOG_LOCAL7
val int_of_facility : facility -> int
val facility_of_string : string -> facility
class type log_destination =
  object
    method close : unit
    method reopen : unit
    method write : level_t -> string -> unit
  end
class log_stderr : log_destination
class logfile :
  ?time_format:(unit -> string) ->
  ?truncate:bool -> string -> log_destination
class syslog : string -> log_destination
class piped_log : string -> log_destination
class logger :
  ?max_level:string ->
  ?destination:log_stderr ->
  unit ->
  object
    val mutable dst : log_stderr
    val mutable max_level : int
    method alert : ('a, unit, string, unit) format4 -> 'a
    method crit : ('b, unit, string, unit) format4 -> 'b
    method debug : ('c, unit, string, unit) format4 -> 'c
    method emerg : ('d, unit, string, unit) format4 -> 'd
    method error : ('e, unit, string, unit) format4 -> 'e
    method info : ('f, unit, string, unit) format4 -> 'f
    method notice : ('g, unit, string, unit) format4 -> 'g
    method printf : level_t -> ('h, unit, string, unit) format4 -> 'h
    method get_destination : unit -> log_destination
    method set_destination : log_destination -> unit
    method set_max_level : string -> unit
    method warning : ('i, unit, string, unit) format4 -> 'i
  end
