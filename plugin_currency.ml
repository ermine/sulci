(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Http_client
open Netconversion
open Xml
open Common
open Unix

type t = {
   nominal: int;
   name: string;
   value: float
}

let curr = ref []

let load_curr () =
   let content = Http_client.request "www.cbr.ru"
		    (Http_client.Get "/scripts/XML_daily.asp") [] in
   let parsed = Xmlstring.parse_string content in
      if match_xml parsed "ValCurs" [] then
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
	    curr := ["RUR", {nominal = 1; name = "Рубль"; value = 1.0}] @ r
      else 
	 curr := []

let get_next_update () =
   let curr_time = gettimeofday () in
   let curr_tm = localtime curr_time in
   let noun, _ = mktime 
      {curr_tm with 
	  tm_sec = 0; tm_min = 0; tm_hour = 11;
	  tm_mday = (if curr_tm.tm_hour < 11 then 
			curr_tm.tm_mday else curr_tm.tm_mday + 1)} 
   in
      (* noun -. curr_time *)
      noun

let _ = 
   load_curr ();
   (* Timer.register load_curr ((get_next_update ()) *. 1000.) 86400000. *)
   Scheduler.add_task load_curr (get_next_update ()) 86400000.

let list_curr =
   let sorted = List.sort (fun (v1, _) (v2, _) ->
			      compare v1 v2
			  ) !curr in
   let rec cycle lst =
      match lst with
	 | [] -> ""
	 | (v, x) :: tail ->
	      (Printf.sprintf "%i %s (%s) = %.4f RUR\n"
		  x.nominal v x.name x.value) ^ cycle tail

   in
      "Котировки Центрального банка РФ\n" ^ cycle sorted

let rex = Pcre.regexp "([0-9]+|[0-9]+\\.[0-9]+)\\s+([a-zA-Z]{3})\\s+([a-zA-Z]{3})"

let currency text event xml out =
   if text = "list" then out (make_msg xml (list_curr))
   else
      try
	 let r = Pcre.exec ~rex text in
	 let amount = Pcre.get_substring r 1 in
	 let amountf = float_of_string amount in
	 let val1 = Pcre.get_substring r 2 in
	 let val2 = Pcre.get_substring r 3 in
	 let val1_x = 
	    let x = try List.assoc (String.uppercase val1) !curr
	    with Not_found ->
	       out (make_msg xml ("Нет такой валюты - " ^ val1));
	       raise Not_found
	    in x.value /. float_of_int x.nominal
	 in
	 let val2_x = 
	    let x = try List.assoc (String.uppercase val2) !curr 
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
		  (Printexc.to_string exn) text;
	      flush Pervasives.stdout;
	      ()

let _ =
   Hooks.register_handle (Hooks.Command ("curr", currency))
