(*
 * (c) 2006 Anastasia Gornostaeva
 *)

{
   type substr_t =
      | Substring of string
      | EOF
}

rule token = parse
   | [^ '&']+
	 { Substring (Lexing.lexeme lexbuf) }
   | "&amp;&lt;"
   | "&lt;"
	 { Substring "<" }
   | "&amp;gt;"
   | "&gt;"
	{ Substring ">" }
   | "&amp;amp;"
   | "&amp;"
	{ Substring "&" }
   | "&amp;apos;"
   | "&apos;"
	{ Substring "'" }
   | "&amp;quot;"
   | "&quot;"
	{ Substring "\"" }
   | "&amp;" (['a'-'z' 'A'-'Z']+ ";" as substr)
	 { Substring ("&" ^ substr) }
   | "&" ['a'-'z' 'A'-'Z']+ ";" as substr
	 { Substring substr }
   | "&amp;" ( '#' ['0'-'9']+ ";" as substr)
	{ Substring ("&" ^ substr) }
   | "&#" ['0'-'9']+ ';' as substr
	 { Substring substr }
   | "&"
	 { Substring "&" }
   | eof
	 { EOF }

{
   let decompose text =
      let buf = Buffer.create (String.length text) in
      let lexbuf = Lexing.from_string text in
      let rec aux_exec () =
	 match token lexbuf with
	    | Substring substr ->
		 Buffer.add_string buf substr;
		 aux_exec ()
	    | EOF ->
		 Buffer.contents buf
      in
	 aux_exec ()
}
