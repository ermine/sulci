(* 
 * (c) 2005-2009 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *
 * Searches potentional url in text and replace it into hyperlink 
 * 
 * TODO: validate query
 *) 

let regexp alpha = ['a'-'z' 'A'-'Z']
let regexp digit = ['0'-'9']
let regexp alphadigit = alpha | digit

let regexp safe = "$" | "-" | "_" | "." | "+"
let regexp extra = "!" | "*" | "'" | "(" | ")" | ","

let regexp national = "{" | "}" | "|" | "\\" | "^" | "~" | "[" | "]" | "`"
let regexp punctuation = "<" | ">" | "#" | "%" | "\""

let regexp unreserved = alpha | digit | safe | extra
let regexp reserved = [';' '/' '?' ':' '@' '&' '=']

let regexp hex = ['A' - 'F' 'a'-'f' '0'-'9']
let regexp escape = "%" hex hex

let regexp uchar = unreserved | escape                                       
let regexp xchar = unreserved | reserved | escape

let regexp hostnumber = digit "." digit "." digit "." digit
let regexp toplabel = alpha | alpha (alphadigit | "-")* alphadigit
let regexp domainlabel = alphadigit | alphadigit (alphadigit | "-")* alphadigit
let regexp hostname = (domainlabel ".")+ toplabel
let regexp host = hostname | hostnumber

let regexp port = digit+
let regexp hostport = host (":" port)?

let regexp wwwhost = ['w''W']['w''W']['w''W'] "."? (domainlabel ".")* toplabel 
let regexp ftphost = ['f''F']['t''T']['p''P'] "."? (domainlabel ".")* toplabel 

let regexp user = (uchar | ":" | "?" | "&" | "=")*
let regexp password = (uchar | ":" | "?" | "&" | "=")*
let regexp urlpath = xchar* 
let regexp login = (user (":" password)? "@")? hostport

let regexp ftptype = "A" | "I" | "D" | "a" | "i" | "d"
let regexp fsegment = ( uchar | "?" | ":" | "@" | "&" | "=" )*
let regexp fpath = fsegment ( "/" fsegment )*

let regexp search = (uchar | ";" | ":" | "@" | "&" | "=")*
let regexp hsegment = (uchar | ";" | ":" | "@" | "&" | "=")*
let regexp hpath = hsegment ("/" hsegment)*

let regexp httpurl =
  "http" "s"? "://" hostport ( "/" hpath ( "?" search )?)?
  | host ":" port ("/" hpath ("?" search)?)?
  | hostport "/" hpath ("?" search)?
  | wwwhost (":" port)? ( "/" hpath ( "?" search )?)?
      
let enclosed lexbuf offset_begin offset_end =
  let len = Ulexing.lexeme_length lexbuf in
    Ulexing.utf8_sub_lexeme lexbuf offset_begin
      (len - (offset_end + offset_begin))
      
let rec do_find_url (callback:string -> string) acc = lexer
  | httpurl ->
      let url = Ulexing.utf8_lexeme lexbuf in
        do_find_url callback (acc ^ callback url) lexbuf
          
  | "(" httpurl ")" ->
      let url = enclosed lexbuf 1 1 in
        do_find_url callback (acc ^ "(" ^ callback url ^ ")") lexbuf
          
  | "ftp://" login ( "/" fpath ( ";type=" ftptype )?)?
  | ftphost ( "/" fpath ( ";type=" ftptype )?)? ->
      let url = Ulexing.utf8_lexeme lexbuf in
        do_find_url callback (acc ^ callback url) lexbuf
          
  | eof ->
      acc
        
  | punctuation
  | national
  | "(" ->
      do_find_url callback (acc ^ Ulexing.utf8_lexeme lexbuf) lexbuf
        
  | _ ->
      Ulexing.rollback lexbuf;
      let skip = skip lexbuf in
        do_find_url callback (acc ^ skip) lexbuf
          
and skip = lexer
  | [^ ' ' '\r' '\n' '\t' '.' ',' '|' ';']+ ->
      Ulexing.utf8_lexeme lexbuf
  | eof ->
      Ulexing.rollback lexbuf;
      ""
  | _ ->
      Ulexing.utf8_lexeme lexbuf
      

(*
let valid_tld =
  [ "aero";
  "biz";
  "cat";
  "com";
  "coop";
  "edu";
  "eu";
  "gov";
  "info";
  "int";
  "jobs";
  "mil";
  "mobi";
  "museum";
  "name";
  "net";
  "org";
  "pro";
  "travel";
  "asia";
  "post";
  "tel";
  "xxx"
  ]
*)

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
       Printf.printf "Orig: %s\n" text;
       Printf.printf "Result: %s\n\n"
   (find_url make_hyperlink text);
       scan s
   in
      scan ["www.ytro.ru";
     "(www.jabber.ru/index.html)";
     "[www.jabber.ru/index.html]";
     "{www.jabber.ru/index.htm}";
     "http://www.ytro.ru";
     "abc http://http://www.ytro.ru. - 20 k";
    " def: www.jabber.ru, ftp.jabber.ru i dr.";
    "http:// abc";
    "http://http://";
    "http://";
    "http://internet.rumus www.jabber.ru - 20k";
    "http://abc.MuSeUm/def.php";
    "(ftp.ir.ru)"]
*)
