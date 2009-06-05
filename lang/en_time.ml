let expand_time_proc _cause years months days hours minutes seconds =
  (if years = 0 then "" else 
     Printf.sprintf "%d %s " years
	     (match years with
	        | 1 -> "year"
	        | _ -> "years")) ^
    (if months = 0 then "" else
	     Printf.sprintf "%d %s " months
	       (match months with
		        | 1 -> "month"
		        | _ -> "months")) ^
    (if days = 0 then "" else
	     Printf.sprintf "%d %s " days
	       (match days with
		        | 1 -> "day"
		        | _ -> "days")) ^
    (if hours = 0 then "" else
	     Printf.sprintf "%d %s " hours
	       (match hours with
		        | 1 -> "hour"
		        | _ -> "hours")) ^
    (if minutes = 0 then "" else
	     Printf.sprintf "%d %s " minutes
	       (match minutes with
		        | 1 -> "minute"
		        | _ -> "minutes")) ^
    (if seconds = 0 && (years <> 0 || months <> 0 || days <> 0 || hours <> 0
			                  || minutes <> 0) then "" else
	     Printf.sprintf "%d %s " seconds
	       (match seconds with
		        | 1 -> "second"
		        | _ -> "seconds"))
    
let float_seconds_proc _cause seconds =
  Printf.sprintf "%.3g seconds" seconds
    
open Lang
    
let _ =
  langtime :=
    LangTime.add "en" 
	    {expand_time = expand_time_proc; 
	     float_seconds = float_seconds_proc}
	    !langtime
      
