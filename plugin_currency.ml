(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Unix
open Xml
open Types
open Config
open Common
open Hooks
open Http_suck

let url = "http://www.cbr.ru/scripts/XML_daily.asp"

type t = {
  nominal: int;
  name: string;
  value: float
}

let curr = ref []

let list_curr = ref ""

let parse_content content =
  let parsed = Xmlstring_netstring.parse_string content in
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
                    {nominal = int_of_string (get_cdata v 
                                                ~path:["Nominal"]);
                     name = get_cdata v ~path:["Name"];
                     value = 
                        let x = get_cdata v ~path:["Value"] in
                        let pos = String.index x ',' in
                          String.set x pos '.';
                          try float_of_string (x) with exn ->
                            log#error "plugin_currency.ml: %s"
                              (Printexc.to_string exn);
                            raise exn
                    }
                 ) z in
        curr := List.sort (fun (v1, _) (v2, _) -> compare v1 v2)
          (["RUR", {nominal = 1; name = "Рубль"; value = 1.0}] @ r);
        let rec cycle lst =
          match lst with
            | [] -> ""
            | (v, x) :: tail ->
                (Printf.sprintf "%i %s (%s) = %.4f RUR\n"
                   x.nominal v x.name x.value) ^ cycle tail
        in
          list_curr :=
            "Котировки Центрального банка РФ (" ^ date ^ ")\n" ^
              cycle !curr
    else 
      curr := []
        
let load_curr () =
  let callback data =
    match data with
      | OK (_media, _charset, content) -> (
          log#info "Plugin_currency: successfully fetched data";
          try
            parse_content content;
          with exn ->
            log#error "Plugin_currency: Unable to parse data: %s"
              (Printexc.to_string exn)
        )
      | Exception exn ->
          ()
  in
    Http_suck.http_get url callback
      
let get_next_update () =
  let curr_time = gettimeofday () in
  let curr_tm = localtime curr_time in
  let noun, _ = mktime 
    {curr_tm with 
       tm_sec = 0; tm_min = 0; tm_hour = 11;
       tm_mday = (if curr_tm.tm_hour < 11 then 
                    curr_tm.tm_mday else curr_tm.tm_mday + 1)} 
  in
    noun
      
let _ = 
  load_curr ();
  Scheduler.add_task Types.timerQ load_curr (get_next_update ())
    (fun () -> get_next_update ())
    
let rex = Pcre.regexp 
  "([0-9]+|[0-9]+\\.[0-9]+)\\s+([a-zA-Z]{3})\\s+([a-zA-Z]{3})"
  
let currency text from xml env out =
  if text = "list" then 
    make_msg out xml !list_curr
  else if text = "refresh" then (
    load_curr ();
    make_msg out xml "sent the request"
  )
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
          make_msg out xml 
            (Lang.get_msg env.env_lang "plugin_currency_no_currency" [val1]);
          raise Not_found
        in x.value /. float_of_int x.nominal
      in
      let val2_x = 
        let x = try List.assoc (String.uppercase val2) !curr 
        with Not_found ->
          make_msg out xml 
            (Lang.get_msg env.env_lang "plugin_currency_no_currency" [val2]);
          raise Not_found
        in x.value /. float_of_int x.nominal
      in
      let result = amountf *. (val1_x /. val2_x) in
        make_msg out xml (Printf.sprintf "%s %s = %.4f %s"
                            amount val1 result val2)
    with
      | Failure "int_of_string" ->
          make_msg out xml 
            (Lang.get_msg env.env_lang "plugin_currency_toobig_number" [])
      | Not_found ->
          ()
      | exn ->
          log#error "plugin_currency.ml (:%s) %s"
            text (Printexc.to_string exn)
            
let _ =
  register_command "curr" currency
