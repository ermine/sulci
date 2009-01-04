(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Pcre
open Xml
open Types
open Common
open Hooks
open Http_suck

(* http://weather.noaa.gov/pub/data/observations/metar/decoded/ULLI.TXT *)

let split_lines lines =
  let map =
    List.find_all 
      (function line ->
         if Pcre.pmatch ~pat: ".+: .+" line then
           true else false)
      lines
  in
    List.map (function m ->
                let c = String.index m ':' in
                  String.sub m 0 c, string_after m (c+2)
             ) map
      
let place_r = regexp "(.+) \\(....\\).+"
let time_r = regexp ".+/ (.+)$"
let temp_r = regexp "(.+) F \\((.+) C\\)"
let wind_r = regexp "(.+):0"
let vis_r = regexp "(.+):0"
  
let parse_weather content =
  let lines = Pcre.split ~pat:"\n" content in
  let line1 = List.hd lines in
  let place = 
    try
      let r = exec ~rex:place_r line1 in
        get_substring r 1
    with Not_found -> line1 in
  let line2 = List.nth lines 1 in
  let time = 
    try
      let r = exec ~rex:time_r line2 in
        get_substring r 1
    with Not_found -> line2 in
  let map = split_lines lines in
  let weather = 
    try List.assoc "Weather" map with _ ->
      try List.assoc "Sky conditions" map with _ -> ""
  in
  let f, c = 
    try 
      let z = List.assoc "Temperature" map in
        try
          let r = exec ~rex:temp_r z in
            get_substring r 1, get_substring r 2
        with Not_found -> "", ""
    with Not_found -> "", ""
  in
  let hum = try List.assoc "Relative Humidity" map with Not_found -> "n/a" in
    (* *)
  let wind = 
    try
      let w = List.assoc "Wind" map in
        try
          let r = exec ~rex:wind_r w in
            get_substring r 1
        with Not_found  -> w
    with _ -> "n/a"
  in
  let vis =
    try
      let v = List.assoc "Visibility" map in
        try
          let r = exec ~rex:vis_r v in
            get_substring r 1
        with Not_found -> v
    with _ -> "n/a"
  in
    Printf.sprintf 
      "%s - %s / %s%sC/%sF, humidity %s, wind: %s, visibility: %s"
      place time (if weather <> "" then weather ^ ", " else "")
      c f hum wind vis
      
let r = Pcre.regexp "[a-zA-Z]{4}"
  
let weather text from xml env out =
  if pmatch ~rex:r text then
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
        make_msg out xml resp
    in
      Http_suck.http_get
        ("http://weather.noaa.gov/pub/data/observations/metar/decoded/" ^
           String.uppercase  text ^ ".TXT")
        callback
  else
    make_msg out xml 
      (Lang.get_msg env.env_lang "plugin_weather_invalid_syntax" [])
      
let _ =
  register_command "wz" weather
