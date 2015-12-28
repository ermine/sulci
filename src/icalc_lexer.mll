{
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
}

let digit = ['0'-'9']
let ident = ['a'-'z' 'A'-'Z']
let ident_num = ['a'-'z' 'A'-'Z' '0'-'9']

rule token = parse
   | [' ' '\t' '\n']           { token lexbuf }
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
   | "max_float"               { MAX_FLOAT }
   | ['p' 'P']['i' 'I']        { PI }
   | "("                       { LPAREN }
   | ")"                       { RPAREN }
   | '='                       { EQ }
   | '!'                       { FACT }
   | ident ident_num* as word  { try let f = Hashtbl.find fun_table word in
				    FUNC f
				 with Not_found -> VAR word
			       }
   | _                         { token lexbuf }
   | eof                       { EOL }
