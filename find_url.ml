(* 
 * (c) 2005 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *
 * Searches potentional url in text and replace it into hyperlink 
 * 
 * TODO: validate query
 *) 

let regexp port = ":" xml_digit+

let regexp schema = ['h''H']['t''T']['t''T']['p''P']['s''S']? 
   | ['f''F']['t''T']['p''P']

let regexp space = [' ' '\n' '\r' '\t']

let regexp subdomain =
   (xml_letter | xml_digit) 
      (xml_letter | xml_digit | '-')* 
      (xml_letter | xml_digit)

let regexp tld = 
   subdomain '.'
      (['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z'] 
      | ['c''C'] ['o''O'] ['m''M']                           (* com *)
      | ['n''N'] ['e''E'] ['t''T']                           (* net *)
      | ['o''O'] ['r''R'] ['g''G']                           (* org *)
      | ['e''E'] ['d''D'] ['u''U']                           (* edu *)
      | ['b''B'] ['i''I'] ['z''Z']                           (* biz *)
      | ['m''M'] ['l''L'] ['i''I']                           (* mil *)
      | ['x''X'] ['x''X'] ['x''X']                           (* xxx *)
      | ['i''I'] ['n''N'] ['t''T']                           (* int *)
      | ['g''G'] ['o''O'] ['v''V']                           (* gov *)
      | ['i''I'] ['n''N'] ['f''F'] ['o''O']                  (* info *)
      | ['m''M'] ['u''U'] ['s''S'] ['e''E'] ['u''U'] ['m''M']) (* museum *)
      
let rec do_find_url (callback:string -> string) acc = lexer
   | schema "://" ->
	let proto = Ulexing.utf8_lexeme lexbuf in
	let host = host lexbuf in
	   if host = "" then
	      let skip = skip lexbuf in
		 do_find_url callback (acc ^ proto ^ skip) lexbuf
	   else
	      let path = query lexbuf in
	      let url = proto ^ host ^ path in
		 if url_end lexbuf then
		    do_find_url callback (acc ^ callback url) lexbuf
		 else
		    do_find_url callback (acc ^ url ^ skip lexbuf) lexbuf
   | ['w''W']['w''W']['w''W'] | ['f''F']['t''T']['p''P'] ->
	Ulexing.rollback lexbuf;
	let host = host lexbuf in
	   if host = "" then
	      let skip = skip lexbuf in
		 do_find_url callback (acc ^ skip) lexbuf
	   else
	      let query = query lexbuf in
	      let url = host ^ query in
		 do_find_url callback (acc ^ callback url) lexbuf
   | eof -> 
	acc
   | _ ->
	do_find_url callback (acc ^ Ulexing.utf8_lexeme lexbuf) lexbuf

and url_end = lexer
   | xml_letter | xml_digit ->
	Ulexing.rollback lexbuf;
	false
   | eof ->
	Ulexing.rollback lexbuf;
	true
   | _ -> 
	Ulexing.rollback lexbuf;
	true

and host = lexer
   | (subdomain '.')* tld port? ->
	Ulexing.utf8_lexeme lexbuf
   | eof ->
	Ulexing.rollback lexbuf;
	""
   | _ ->
	Ulexing.rollback lexbuf;
	""

and query = lexer
   | "/" ->
	let path = "/" ^ path "" lexbuf in
	   path
   | eof -> 
	Ulexing.rollback lexbuf;
	""
   | _ ->   
	Ulexing.rollback lexbuf;
	""

and path acc = lexer
   | ['.' ',' '?' '!' ':' ';' '(' ')' '-''|']+ (space | eof) ->
	Ulexing.rollback lexbuf;
	acc
   | [ '<' '>'] | "&lt;" | "&gt;"  ->
	Ulexing.rollback lexbuf;
	acc
   | space ->
	Ulexing.rollback lexbuf;
	acc
   | eof ->
	Ulexing.rollback lexbuf;
	acc
   | _ ->
	path (acc ^ Ulexing.utf8_lexeme lexbuf) lexbuf

and skip = lexer
   | [^ ' ' '\r' '\n' '\t' '.' ',' '|' ';']+ ->
	Ulexing.utf8_lexeme lexbuf
   | eof ->
	Ulexing.rollback lexbuf;
	""
   | _ ->
	Ulexing.utf8_lexeme lexbuf

let find_url callback text =
   let lexbuf = Ulexing.from_utf8_string text in
      do_find_url callback "" lexbuf

let compare str1 str2 =
   if String.length str1 > String.length str2 &&
      String.sub str1 0 (String.length str2) = str2 then
	 true
   else
      false

let make_hyperlink url =
   if compare url "http://" || 
      compare url "https://" || 
      compare url "ftp://" then
	 "<a href='" ^ url ^ "'>" ^ url ^ "</a>"
   else
      if compare url "ftp" then
	 "<a href='ftp://" ^ url ^ "'>" ^ url ^ "</a>"
      else
	 "<a href='http://" ^ url ^ "'>" ^ url ^ "</a>"

(*
let _ =
   let rec scan list =
      match list with
	 | [] -> ()
	 | text :: s ->
	      print_endline (find_url make_hyperlink text);
	      scan s
   in
      scan ["www.ytro.ru";
	    "http://www.ytro.ru";
	    "abc http://http://www.ytro.ru. - 20 k";
	   " def: www.jabber.ru, ftp.jabber.ru i dr.";
	   "http:// abc";
	   "http://http://";
	   "http://";
	   "http://internet.rumus www.jabber.ru - 20k";
	   "http://abc.MuSeUm/def.php";
	   "ftp.i.ru"]
*)
