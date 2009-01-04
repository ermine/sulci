%{
  (*
   * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
   *)

  open Printf
%}

%token <float> NUM
%token PLUS MINUS MUL DIVIDE MOD CARET UMINUS
%token COS SIN ACOS ASIN COSH SINH TAN ATAN TANH ATAN2 CEIL FLOOR
%token LOG LOG10 EXP SQRT FACT FIB
%token MAX_FLOAT PI
%token EOL

%start line
%type <string> line

%% 
line:
| expr EOL        { sprintf "%.10g" $1 }
 
expr:
| NUM            { $1 }
| expr expr PLUS   { $1 +. $2 }
| expr expr MINUS  { $1 -. $2 }
| expr expr MUL    { $1 *. $2 }
| expr expr DIVIDE { $1 /. $2 }
| expr expr MOD    { mod_float $1 $2 }
| expr expr CARET  { $1 ** $2 }
| expr UMINUS     { -. $1 }
| expr SQRT       { sqrt $1 }
| expr EXP        { exp $1 }
| expr LOG        { log $1 }
| expr LOG10      { log10 $1 }
| expr COS        { cos $1 }
| expr SIN        { sin $1 }
| expr TAN        { tan $1 }
| expr ACOS       { acos $1 }
| expr ASIN       { asin $1 }
| expr ATAN       { atan $1 }
| expr expr ATAN2 { atan2 $1 $2 }
| expr COSH       { cosh $1 }
| expr SINH       { sinh $1 }
| expr TANH       { tanh $1 }
| expr CEIL       { ceil $1 }
| expr FLOOR      { floor $1 }
| expr FACT       { Math.fact $1 }
| expr FIB        { Math.fib $1 }
| MAX_FLOAT       { max_float }
| PI              { 4. *. (atan (1./.2.) +. atan (1./.3.)) }

%%
