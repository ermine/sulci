(*
 * (c) 2004-2010 Anastasia Gornostaeva
 *)

open Unix
open Light_xml
open Common
open Hooks
open Plugin_command
open Http_suck
open Plugin_scheduler

let url = "http://www.cbr.ru/scripts/XML_daily.asp"

type t = {
  nominal: int;
  name: string;
  value: float
}

let curr = ref []

let list_curr = ref ""

let parse_content content =
  let parsed = Conv_xml.parse_document content in
    if match_xml parsed "ValCurs" [] then
      let date = get_attr_s parsed "Date" in
      let vals = get_subels parsed in
      let  z = List.find_all (function 
                                | Xmlelement _ -> true
                                | Xmlcdata _ -> false
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
      | Exception _exn ->
          log#error "plugin_currency: Unable to fetch currency data"
  in
    Http_suck.http_get url callback
      
let rex = Pcre.regexp 
  "([0-9]+|[0-9]+\\.[0-9]+)\\s+([a-zA-Z]{3})\\s+([a-zA-Z]{3})"
  
let currency xmpp env kind jid_from text =
  if text = "list" then 
    env.env_message xmpp kind jid_from !list_curr
  else if text = "refresh" then (
    load_curr ();
    env.env_message xmpp kind jid_from "sent the request"
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
          env.env_message xmpp kind jid_from 
            (Lang.get_msg env.env_lang "plugin_currency_no_currency" [val1]);
          raise Not_found
        in x.value /. float_of_int x.nominal
      in
      let val2_x = 
        let x = try List.assoc (String.uppercase val2) !curr 
        with Not_found ->
          env.env_message xmpp kind jid_from 
            (Lang.get_msg env.env_lang "plugin_currency_no_currency" [val2]);
          raise Not_found
        in x.value /. float_of_int x.nominal
      in
      let result = amountf *. (val1_x /. val2_x) in
        env.env_message xmpp kind jid_from (Printf.sprintf "%s %s = %.4f %s"
                            amount val1 result val2)
    with
      | Failure "int_of_string" ->
          env.env_message xmpp kind jid_from 
            (Lang.get_msg env.env_lang "plugin_currency_toobig_number" [])
      | Not_found ->
          env.env_message xmpp kind jid_from "?"
      | exn ->
          env.env_message xmpp kind jid_from "?";
          log#error "plugin_currency.ml (:%s) %s"
            text (Printexc.to_string exn)
            
let plugin opts =
  add_for_token
    (fun _opts xmpp ->
       add_commands xmpp [("curr", currency)] opts
    );
  load_curr ();
  let t =
    try List.assoc "time" (List.assoc "refresh" opts)
    with Not_found -> "11:00" in
  let (hour, min) =
    try Scanf.sscanf t "%d:%d" (fun hoir min -> (hoir, min))
    with Scanf.Scan_failure str ->
      raise (Plugin.Error
               (Printf.sprintf "Invalid option refresh: %s" str))
  in
  let _ = Scheduler.add_task timerQ load_curr
    (get_next_time hour min ()) (get_next_time hour min) in
    ()
      
let () =
  Plugin.add_plugin "currency" plugin
