{
  (*
   * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
   *)

  open Pcalc
}

let digit = ['0'-'9']

rule token = parse
  | [' ' '\t']                { token lexbuf }
  | digit+
  | "." digit+
  | digit+ "." digit+
  | digit+ ("." digit)* ("e"|"E")('-'|'+')? digit+ as num
      { NUM (float_of_string num) }
  | '0' ('x'|'X') ['A'-'F' 'a'-'f' '0'-'9']+ as num
      { NUM (float_of_string num) }
  | '0' ['b' 'B'] ['0' '1']+ as num
      { NUM (float_of_int (int_of_string num)) }
  | '0' ['o' 'O'] ['0'-'8']+ as num
      { NUM (float_of_int (int_of_string num)) }
  | '+'                       { PLUS }
  | '-'                       { MINUS }
  | '*'                       { MUL }
  | '/'                       { DIVIDE }
  | '%'                       { MOD }
  | '^'                       { CARET }
  | 'n'                       { UMINUS }
  | "sqrt"                    { SQRT }
  | "exp"                     { EXP }
  | "log"                     { LOG }
  | "log10"                   { LOG10 }
  | "cos"                     { COS }
  | "sin"                     { SIN }
  | "tan"                     { TAN }
  | "asoc"                    { ACOS }
  | "asin"                    { ASIN }
  | "atan"                    { ATAN }
  | "atab2"                   { ATAN2 }
  | "cosh"                    { COSH }
  | "sinh"                    { SINH }
  | "tanh"                    { TANH }
  | "ceil"                    { CEIL }
  | "floor"                   { FLOOR }
  | "fact"                    { FACT }
  | "fib"                     { FIB }
  | "max_float"               { MAX_FLOAT }
  | ['p' 'P'] ['i' 'I']       { PI }
  | _                         { token lexbuf }
  | eof                       { EOL }
      
