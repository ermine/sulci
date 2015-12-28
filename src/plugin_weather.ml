(*
 * (c) 2004-2010 Anastasia Gornostaeva
 *)

open Common
open Hooks
open Plugin_command
open Http_suck

(* http://weather.noaa.gov/pub/data/observations/metar/decoded/ULLI.TXT *)

let split_lines contents =
  let rec aux_split q acc =
    let l =
      try Some (String.index_from contents q '\n')
      with Not_found -> None
    in
      match l with
        | Some v ->
            aux_split (v+1) (String.sub contents q (v-q) :: acc)
        | None ->
            List.rev (String.sub contents q (String.length contents - q) :: acc)
  in
    aux_split 0 []
  
let get_fields lines =
  List.fold_left (fun acc x ->
                    let v =
                      try
                        let q = String.index x ':' in
                        let field = String.sub x 0 q in
                        let value = String.sub x (q+2) (String.length x -q-2) in
                        let fname =
                          match field with
                            | "Wind" -> "wind"
                            | "Visibility" -> "visibility"
                            | "Sky conditions" -> "sky"
                            | "Weather" -> "weather"
                            | "Temperature" -> "temperature"
                            | "Dew Point" -> "dewpoint"
                            | "Relative Humidity" -> "humidity"
                            | "Pressure (altimeter)" -> "pressure"
                            | "ob" -> "ob"
                            | _ ->
                                raise Not_found
                        in
                          Some (fname, value)
                      with Not_found ->
                        None
                    in
                      match v with
                        | Some f ->
                            f :: acc
                        | None ->
                            acc
                 ) [] lines

let remove_zero str =
  let i = String.length str in
    if i > 2 && str.[i-1] = '0' && str.[i-2] = ':' then
      String.sub str 0 (i-2)
    else
      str

let parse_temperature str =
  let rec temper = parser
    | [< f = get_number; '' '; ''F'; '' '; ''('; c = get_number; '' ';
         ''C'; >] ->
        Some (f, c)
    | [< rest >] ->
        None
  and get_number = parser
    | [< ''-'; t = get_digits 0; rest >] ->
        (-t)
    | [< t = get_digits 0; rest >] ->
        t
    | [< >] ->
        0
  and get_digits acc = parser
    | [< ' ('0'..'9') as c; rest >] ->
        let i = int_of_char c - 48 in
          get_digits (acc * 10 + i) rest
    | [< >] ->
        acc
  in
    temper (Stream.of_string str)

let parse_weather content =
  match split_lines content with
    | line1 :: line2 :: rest ->
        let map = get_fields rest in
        let place = 
          try
            let q = String.index line1 '(' in
              String.sub line1 0 (q-1) 
          with Not_found -> line1 in
        let time = 
          try
            let q = String.index line2 '/' in
              String.sub line2 (q+2) (String.length line2 - q - 2)
          with Not_found -> line2 in
        let weather = 
          try List.assoc "weather" map with _ ->
            try List.assoc "sky" map with _ -> ""
        in
        let temperature = 
          try 
            let z = List.assoc "temperature" map in
            let data = parse_temperature z in
              data
          with Not_found -> None
        in
        let humidity = try List.assoc "humidity" map with Not_found -> "n/a" in
        let pressure = try List.assoc "pressure" map with Not_found -> "n/a" in
        let wind = 
          try let w = List.assoc "wind" map in remove_zero w
          with _ -> "n/a"
        in
        let visibility =
          try let v = List.assoc "visibility" map in remove_zero v
          with _ -> "n/a"
        in
          Printf.sprintf
            "%s - %s / %s%s%shumidity: %s, pressure: %s, wind: %s, visibility: %s"
            place time weather (if weather <> "" then ", " else "")
            (match temperature with
               | Some (f, c) -> Printf.sprintf "%d°C / %d°F, " c f
               | None -> "")
            humidity pressure wind visibility
    | _ ->
        "n/a"
          
let is_noaa_code str =
  if String.length str = 4 then
    let rec aux_check i =
      if i < 4 then
        match str.[i] with
          | 'a'..'z'
          | 'A'..'Z' ->
              aux_check (succ i)
          | _ -> false
      else
        true
    in
      aux_check 0
  else
    false
  
let weather xmpp env kind jid_from text =
  if is_noaa_code text then
    let callback data =
      let resp = match data with
        | OK (_media, _charset, body) -> (
            try
              parse_weather body
            with _exn ->
              Lang.get_msg env.env_lang "plugin_weather_not_parsed" []
          )
        | Exception exn ->
            match exn with 
              | ClientError ->
                  Lang.get_msg env.env_lang "plugin_weather_404" []
              | ServerError ->
                  Lang.get_msg env.env_lang "plugin_weather_server_error" []
              | _ ->
                  Lang.get_msg env.env_lang "plugin_weather_server_error" []
      in
        env.env_message xmpp kind jid_from resp
    in
      Http_suck.http_get
        ("http://weather.noaa.gov/pub/data/observations/metar/decoded/" ^
           String.uppercase  text ^ ".TXT")
        callback
  else
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_weather_invalid_syntax" [])
      

let plugin opts =
  add_for_token
    (fun _opts xmpp ->
       add_commands xmpp [("wz", weather)] opts
    )

let _ =
  Plugin.add_plugin "weather" plugin
