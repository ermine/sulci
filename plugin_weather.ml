open Xml
open Types
open Http_client

(* http://weather.noaa.gov/pub/data/observations/metar/decoded/ULLI.TXT *)

let split_lines lines =
   let map = List.find_all 
		(function line ->
		    if Str.string_match (Str.regexp ".+: .+") line 0 then
		       true else false)
		lines in
      List.map (function m ->
		   let m1 = Str.bounded_split (Str.regexp ": ") m 2 in
		      (List.hd m1, List.nth m1 1)
	       ) map

let get_weather code =
   let content = 
      Http_client.request "weather.noaa.gov"
	 (Get ("/pub/data/observations/metar/decoded/" ^ code ^ ".TXT"))
	 [] in
      
   let lines = Str.split (Str.regexp "\n") content in

   let line1 = List.hd lines in
   let place = 
      if Str.string_match (Str.regexp "\\(.+\\) (....).+") line1 0 then
	 Str.matched_group 1 line1 else line1 in
   let line2 = List.nth lines 1 in
   let time = 
      if Str.string_match (Str.regexp ".+/ \\(.+\\)$") line2 0 then
	 Str.matched_group 1 line2 else line2 in
   let map = split_lines lines in
   let weather = 
      try List.assoc "Weather" map with _ ->
	 List.assoc "Sky conditions" map
   in
   let f, c = 
      let z = List.assoc "Temperature" map in
	 if Str.string_match (Str.regexp "\\(.+\\) F (\\(.+\\) C)") z 0 then
	    Str.matched_group 1 z, Str.matched_group 2 z
	 else
	    "", "" in
   let hum = List.assoc "Relative Humidity" map in
   let wind = 
      try
	 let w = List.assoc "Wind" map in
	    if Str.string_match (Str.regexp "\\(.+\\):0") w 0 then
	       Str.matched_group 1 w
	    else w
      with _ -> "n/a"
   in
   let vis =
      try
	 let v = List.assoc "Visibility" map in
	    if Str.string_match (Str.regexp "\\(.+\\):0") v 0 then
	       Str.matched_group 1 v
	    else v
      with _ -> "n/a"
   in

      Printf.sprintf 
	 "%s - %s / %s, %sC/%sF, humidity %s wind: %s visibility: %s"
	 place time weather c f hum wind vis

let rex = Str.regexp "wz \\([a-zA-Z][a-zA-Z][a-zA-Z][a-zA-Z]\\)"

let weather xml out bot mynick lang =
   let body = try Xml.get_cdata xml ~path:["body"] with _ -> "" in
      if Str.string_match rex body 0 then 
	 let code = Str.matched_group 1 body in
	 let proc (xml, out) =
	    let response = 
	       try get_weather (String.uppercase code) with _ -> 
		  "Undefined error" in
	       out (make_msg xml response)
	 in
	    ignore (Thread.create proc (xml, out))

let _ =
   Muc.register_cmd "wz" weather;
   Muc.register_help "wz"
"wz code
   Вывод погоды по коду NOAA"
