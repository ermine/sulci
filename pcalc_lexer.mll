{
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
   | '+'                       { PLUS }
   | '-'                       { MINUS }
   | '*'                       { MUL }
   | '/'                       { DIVIDE }
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
   | "max_float"               { MAX_FLOAT }
   | ['p' 'P'] ['i' 'I']       { PI }
   | _                         { token lexbuf }
   | eof                       { EOL }
