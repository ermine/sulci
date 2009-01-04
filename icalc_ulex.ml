(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Icalc

let create_hashtable size init =
  let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init; 
    tbl

let fun_table =
  create_hashtable 16 [
    ("sin", sin);
    ("cos", cos);
    ("tan", tan);
    ("asin", asin);
    ("acos", acos);
    ("atan", atan);
    ("cosh", cosh);
    ("sinh", sinh);
    ("tanh", tanh);
    ("log", log);
    ("log10", log10);
    ("exp", exp);
    ("sqrt", sqrt);
    ("fib", Math.fib)
  ]

let regexp ident_char = xml_letter | xml_digit | '_' | xml_combining_char | xml_extender
let regexp ident_first = xml_letter | '_' | xml_combining_char | xml_extender

let rec token = lexer
  | [' ' '\t' '\n']  ->
      token lexbuf
  | xml_digit+
  | "." xml_digit+
  | xml_digit+ "." xml_digit+
  | xml_digit+ ("." xml_digit)* ("e"|"E")('-'|'+')? xml_digit+ ->
      let num = Ulexing.utf8_lexeme lexbuf in
        NUM (float_of_string num)
  | '0' ('x'|'X') ['a'-'f' 'A'-'F' '0'-'9']+ ->
      NUM (float_of_string (Ulexing.utf8_lexeme lexbuf))
  | '0' ['b' 'B'] ['0' '1']+ ->
      NUM (float_of_int (int_of_string (Ulexing.utf8_lexeme lexbuf)))
  | '0' ['o' 'O'] ['0'-'8']+ ->
      NUM (float_of_int (int_of_string (Ulexing.utf8_lexeme lexbuf)))
  | '+' ->
      PLUS
  | '-' ->
      MINUS
  | '*' ->
      MUL
  | '/' ->
      DIVIDE
  | '%' ->
      MOD
  | '^'  ->
      CARET
  | "max_float" ->
      MAX_FLOAT
  | ['p' 'P']['i' 'I'] ->
      PI
  | "(" ->
      LPAREN
  | ")" ->
      RPAREN
  | '=' ->
      EQ
  | '!' ->
      FACT
  | ident_first ident_char* ->
      let word = Ulexing.utf8_lexeme lexbuf in
        (try let f = Hashtbl.find fun_table word in
           FUNC f
         with Not_found -> VAR word)
          
  | eof ->
      EOL
  | _ ->
      token lexbuf
        
