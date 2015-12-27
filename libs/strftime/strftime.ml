(*
 * (c) 2004-2009 Anastasia Gornostaeva
 *)

open Unix

let strftime ?(unixtime=0.0) ?tm ?(tz=0) fmt:string =
  let td = 
    if unixtime <> 0.0 then
	    let tz_ = float_of_int (tz * 3600) in
	      Unix.gmtime (unixtime +. tz_);
    else
	    match tm with
	      | None -> gmtime ((time ()) +. float (tz * 3600))
	      | Some d -> d         (* !!! adjuct TZ *)
  in
  let buffer = Buffer.create 10 in
  let rec tm_parse pos =
    let ch = String.get fmt pos in
	    if ch = '%' then
	      let w = match String.get fmt (pos+1) with
	        | 'D' -> Printf.sprintf "%02d/%02d/%02d"
		          (td.tm_mon+1) td.tm_mday (td.tm_year mod 100)
	        | 'd' -> Printf.sprintf "%02d" td.tm_mday
	        | 'H' -> Printf.sprintf "%02d" td.tm_hour
	        | 'M' -> Printf.sprintf "%02d" td.tm_min
	        | 'm' -> Printf.sprintf "%02d" (td.tm_mon+1)
	        | 'S' -> Printf.sprintf "%02d" td.tm_sec
	        | 's' -> Printf.sprintf "%d"   (int_of_float unixtime)
	        | 'T' -> Printf.sprintf "%02d:%02d:%02d"
		          td.tm_hour td.tm_min td.tm_sec
	        | 't' -> "\t"
	        | 'Y' -> Printf.sprintf "%4d" (td.tm_year+1900)
	        | 'y' -> Printf.sprintf "%02d" (td.tm_year mod 100)
	        | '%' -> "%"
	        | _ -> String.sub fmt pos 2
	      in
	        Buffer.add_string buffer w;
	        tm_parse (pos+2)
	    else
	      begin
	        Buffer.add_char buffer ch;
	        tm_parse (pos+1);
	      end;
  in
    (try tm_parse 0 with _ -> ());
    Buffer.contents buffer
      
let seconds_to_string seconds =
  if seconds >= 60 then
    let mins = seconds / 60 in
    let secs = seconds - (mins * 60) in
	    if mins >= 60 then
	      let hours = mins / 60 in
	      let mins1 = mins - (hours * 60) in
	        if hours >= 24 then
		        let days = hours / 24 in
		        let hours1 = hours - (days * 24) in
		          if days >= 30 then
			          let month = days / 30 in
			          let days1 = days - (month * 30) in
			            if month >= 12 then
			              let year = month / 12 in
			              let month1 = month - (year * 12) in
				              year, month1, days1, hours1, mins1, secs
			            else
			              0, month, days1, hours1, mins1, secs
		          else
			          0, 0, days, hours1, mins1, secs
	        else
		        0, 0, 0, hours, mins1, secs
	    else
	      0, 0, 0, 0, mins, secs
  else 
    0, 0, 0, 0, 0, seconds
