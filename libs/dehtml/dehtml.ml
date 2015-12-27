(*
 * (c) 2006 Anastasia Gornostaeva
 *)

open Nethtml
open Netconversion

let draw_table cols (data:(int * string) list list) =
  let tabs = Array.make cols 0 in
  let rec aux_row i rest =
    match rest with
	    | [] -> ()
	    | (colspan, c) :: xs ->
	        if colspan > 1 then
		        let len = (String.length c - colspan + 1) / colspan in
		        let len1 = len + ((String.length c - colspan + 1) mod colspan) 
		        in
		          tabs.(i) <- max tabs.(i) len1;
		          for j = (i+1) to i + colspan - 1 do
		            tabs.(j) <- max tabs.(j) len;
		          done;
	        else
		        tabs.(i) <- max tabs.(i) (String.length c);
	        aux_row (i + colspan) xs
  in
  let tablen i s =
    let len = ref 0 in
	    for j = i to i + s - 1 do
	      len := !len + tabs.(j);
	    done;
	    !len
  in
  let rec aux_draw_row i rest acc =
    match rest with
	    | [] -> acc
	    | (colspan, c) :: xs ->
	        if colspan = 1 then
		        aux_draw_row (i+1) xs 
		          (acc ^ c ^ 
			           (String.make (tabs.(i) - String.length c) ' ') ^ " ")
	        else
		        aux_draw_row (i + colspan) xs
		          (acc ^ c ^ 
			           (String.make ((tablen i colspan) + 
					                       (colspan-1) - String.length c) 
				            ' ') ^ " ")
  in
    List.iter (fun r -> aux_row 0 r) data;
    String.concat "\n"
	    (List.map (fun r -> aux_draw_row 0 r "") data)
      
let rec process_element el =
  let rec aux_iter els acc =
    match els with
      | [] -> String.concat "" (List.rev acc)
      | h::t -> 
          match h with
            | Element (_tag, _, _) -> 
		            aux_iter t ((process_element h) :: acc)
            | Data data -> 
		            let text =
			            if data = "\n" then
			              ""
			            else if data <> "" &&
			              data.[String.length data - 1] = '\n' then
			                String.sub data 0 (String.length data - 1)
			            else
			              data
		            in
			            aux_iter t (text :: acc)
  in
  let table subels =
    let cols data =
	    let rec aux_cols rest acc (colspans:int) =
	      match rest with
	        | [] -> colspans, List.rev acc
	        | x :: xs ->
		          match x with
		            | Element (_, attrs, data) ->
			              let cspan = try 
			                int_of_string (List.assoc "colspan" attrs) with
				                  Not_found -> 1 in
			                aux_cols xs ((cspan, aux_iter data []) :: acc)
				                (colspans + cspan)
		            | Data _ ->
			              aux_cols xs acc colspans
	    in
	      aux_cols data [] 0
    in
    let max_cols = ref 0 in
    let rows data =
	    let rec aux_rows rest acc =
	      match rest with
	        | [] -> List.rev acc
	        | x :: xs ->
		          match x with
		            | Element (_, _, data) ->
			              let cs, items = cols data in
			                max_cols := max !max_cols cs;
			                aux_rows xs (items :: acc)
		            | Data _ ->
			              aux_rows xs acc
	    in
	      aux_rows data []
    in
	    draw_table !max_cols (rows subels)
  in
    match el with
      | Element (tag, attrs, subels) -> begin
	        match tag with
		        | "a" ->
		            let cdata = aux_iter subels [] in
			            (if cdata <> "" then 
			               cdata ^ " "
			             else "") ^
			              (try "(" ^ 
				               (List.assoc "href" attrs) ^ ")"
			               with Not_found -> "")
		        | "br" -> 
		            "\n"
		        | "div"
		        | "p" ->
		            let str = aux_iter subels [] in
			            if str <> "" then
			              if str.[String.length str-1] = '\n' then
			                "\n" ^ str
			              else
			                "\n" ^ str ^ "\n"
			            else
			              ""
		        | "table" ->
		            table subels
		        | "li" -> 
		            let str = aux_iter subels [] in
			            if str <> "" &&
			              str.[String.length str - 1] = '\n' then
			                " * " ^ str
			            else
			    " * " ^ str ^ "\n"
		        | "form"
		        | "img"
		        | "script" ->
		            ""
		        | _ ->
		            aux_iter subels []
	      end
      | Data data ->
	        if data = "\n" then
		        ""
	        else if data <> "" &&
		        data.[String.length data - 1] = '\n' then
		          String.sub data 0 (String.length data - 1)
	        else
		        data

(*
let rex = Pcre.regexp "([<>&'\"])|([ \t]*\n)+"

  let xml_encode text =
  Pcre.substitute_substrings ~rex
  ~subst:(fun x ->
	try
  match Pcre.get_substring x 1 with
	| "&" -> "&amp;"
	| "'" -> "&apos;"
	| "\"" -> "&quot;"
	| "<" -> "&lt;"
	| ">" -> "&gt;"
	| z ->  z
	with Not_found ->
	"\n"
	) text
*)
(*
  let esc_ent = Pcre.regexp 
  "(&amp;([A-Za-z]+;)|(#['0'-'9']+;))|(&(lt|gt|quot|amp|apos);)"
(* "&((amp;([a-zA-Z]+)|(#[0-9]+))|(lt|gt|amp|apos|quot));" *)

  let decode_amp text =
  let occurences =
  try Pcre.exec_all ~rex:esc_ent text with Not_found -> [||] 
  in
  if occurences <> [||] then
	let buf = Buffer.create (String.length text) in
	let n = ref 0 in
	for k = 0 to Array.length occurences - 1 do
  let (n0,n1) = Pcre.get_substring_ofs (occurences.(k)) 0 in
  if n0 > !n then
  Buffer.add_string buf (String.sub text !n (n0 - !n));
  let occurence = occurences.(k) in
	let replacement = 
	try
	match Pcre.get_substring occurence 3 with 
	| "lt" -> "<"
	| "gt" -> ">"
	| "amp" -> "&"
	| "apos" -> "'"
	| "quot" -> "\""
	| other -> "&" ^ other ^ ";"
	with Not_found ->
	try
	"& " ^ Pcre.get_substring occurence 4 ^ ";"
	with Not_found ->
	try
	match Pcre.get_substring occurence 5 with
	| "lt" -> "<"
	| "gt" -> ">"
	| "amp" -> "&"
	| "apos" -> "'"
	| "quot" -> "\""
	| other -> "&" ^ other ^ ";"
	with Not_found -> ""
	in
	Buffer.add_string buf replacement;
  n := n1;
  done;
  let n0 = String.length text in
  if n0 > !n then
  Buffer.add_string buf (String.sub text !n (n0 - !n));
  Buffer.contents buf
  else text

*)
(*
  let decode_amp text = 
  Pcre.substitute_substrings ~rex:esc_ent
  ~subst:(fun x ->
	try
	match Pcre.get_substring x 2 with
	| "lt;" -> "<"
	| "gt;" -> ">"
	| "quot;" -> "\""
	| "apos;" -> "'"
	| "amp;" -> "&"
	| other -> "&" ^ other
	with Not_found ->
	try
	let num = Pcre.get_substring x 3 in
	"&" ^ num
	with Not_found ->
	match Pcre.get_substring x 5 with
	| "lt" -> "<"
	| "gt" -> ">"
	| "quot" -> "\""
	| "apos" -> "'"
	| "amp" -> "&"
	| other -> other
	) text

*)
let decode_amp text =
  Rlex1.decompose text

let html2text string =
  let ch = new Netchannels.input_string (decode_amp string) in
  let doc = parse ~dtd:relaxed_html40_dtd ~return_declarations:false
	    ~return_pis:false ~return_comments:false ch in
  let s = String.concat "" (List.map (fun d -> process_element d) doc) in
    Rlex2.decode s
