(*
 * (c) 2006-2008 Anastasia Gornostaeva
 * 
 * LOG_FILE returns filename
 * LOG_LINE returns line number
 * LOG_LOC "somestring" is same for "%s:%d: something" FILE LINE
 *)

open Camlp4.PreCast
open Syntax

let _loc = Loc.ghost

EXTEND Gram
GLOBAL: expr;

  expr: LEVEL "simple"
    [[ "LOG_FILE" ->
	       let filename = Loc.file_name _loc in
	         <:expr< $str:filename$ >>
     | "LOG_LINE" ->
	       let lineno = Loc.start_line _loc in
	         <:expr< $int:string_of_int lineno$ >>
     | "LOG_LOC"; fmt = STRING ->
	       let filename = Loc.file_name _loc in
	       let lineno = Loc.start_line _loc in
	       let fmt' = filename ^ ":" ^ string_of_int lineno ^ ": " ^ fmt in
	         <:expr< $str:fmt'$ >>
     ]];
END
