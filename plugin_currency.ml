(*                                                                          *)
(* (c) 2004, Anastasia Gornostaeva. <ermine@ermine.pp.ru                    *)
(*                                                                          *)

open Http_client
open Netconversion
open Xml
open Types

type t = {
   nominal: int;
   name: string;
   value: float
}

let curr =
   let content = Http_client.request "www.cbr.ru"
		    (Http_client.Get "/scripts/XML_daily.asp") [] in
   let parsed = Xmlstring.parse_string content in
      match_xml parsed "ValCurs";
      let date = get_attr_s parsed "Date" in
      let vals = get_subels parsed in
      let  z = List.find_all (function 
				 | Xmlelement _ -> true
				 | _ -> false
			     ) vals in
      let r = 
	 List.map (function v ->
		      get_cdata v ~path:["CharCode"],
		      {nominal = int_of_string (get_cdata v ~path:["Nominal"]);
		       name = get_cdata v ~path:["Name"];
		       value = 
			  let x = get_cdata v ~path:["Value"] in
			  let pos = String.index x ',' in
			     String.set x pos '.';
			     try float_of_string (x) with exn ->
				Printf.printf "%s\n" x; raise exn
		      }
		  ) z in
	 ["RUR", {nominal = 1; name = "Рубль"; value = 1.0}] @ r

let list_curr =
   let sorted = List.sort (fun (v1, _) (v2, _) ->
			      compare v1 v2
			  ) curr in
   let rec cycle lst =
      match lst with
	 | [] -> ""
	 | (v, x) :: tail ->
	      (Printf.sprintf "%i %s (%s) = %.4f RUR\n"
		  x.nominal v x.name x.value) ^ cycle tail

   in
      "Котировки Центрального банка РФ\n" ^ cycle sorted

let rex = Str.regexp "curr +\\([0-9]+\\|[0-9]+\\.[0-9]+\\) +\\([a-zA-Z][a-zA-Z][a-zA-Z]\\) +\\([a-zA-Z][a-zA-Z][a-zA-Z]\\)"

let rex_cmd = Str.regexp "curr \\([a-z]+\\)"

let currency xml out bot mynick lsnh =
   let body = try Xml.get_cdata xml ~path:["body"] with _ -> "" in
      if Str.string_match rex body 0 then
	 try
	    let amount = Str.matched_group 1 body in
	    let amountf = float_of_string amount in
	    let val1 = Str.matched_group 2 body in
	    let val2 = Str.matched_group 3 body in
	    let val1_x = 
	       let x = try List.assoc (String.uppercase val1) curr 
	       with Not_found ->
		  out (make_msg xml ("Нет такой валюты - " ^ val1));
		  raise Not_found
	       in x.value /. float_of_int x.nominal
	    in
	    let val2_x = 
	       let x = try List.assoc (String.uppercase val2) curr 
	       with Not_found ->
		  out (make_msg xml ("Нет такой валюты - " ^ val2));
		  raise Not_found
	       in x.value /. float_of_int x.nominal
	    in
	    let result = amountf *. (val1_x /. val2_x) in
	       out (make_msg xml (Printf.sprintf "%s %s = %.4f %s"
				     amount val1 result val2))
	 with
	    | Failure "int_of_string" ->
		 out (make_msg xml "Таких денег не бывает!")
	    | exn ->
		 Printf.printf "exception %s: [%s]\n" 
		     (Printexc.to_string exn) body;
		 flush Pervasives.stdout;
		 ()

      else if Str.string_match rex_cmd body 0 then
	 let cmd = Str.matched_group 1 body in
	    match cmd with
	       | "list" ->
		    out (make_msg xml (list_curr))
	       | other ->
		    ()

let _ =
   Muc.register_cmd "curr" currency;
   Muc.register_help "curr"
"curr list
   Вывод списка котировок валют ЦБ РФ
curr число валюта1 валюта2
   Калькулятор для перевода валюты1 в валюту2 по кросс-курсу ЦБ РФ"
