let expand_time_proc cause years months days hours minutes seconds =
   (if years = 0 then "" else 
       Printf.sprintf "%d %s " years
	  (match years with
	      | 1 -> "año"
	      | _ -> "años")) ^
      (if months = 0 then "" else
	  Printf.sprintf "%d %s " months
	     (match months with
		 | 1 -> "mes"
		 | _ -> "meses")) ^
      (if days = 0 then "" else
	  Printf.sprintf "%d %s " days
	     (match days with
		 | 1 -> "día"
		 | _ -> "dias")) ^
      (if hours = 0 then "" else
	  Printf.sprintf "%d %s " hours
	     (match hours with
		 | 1 -> "hora"
		 | _ -> "horas")) ^
      (if minutes = 0 then "" else
	  Printf.sprintf "%d %s " minutes
	     (match minutes with
		 | 1 -> "minuto"
		 | _ -> "minutos")) ^
      (if seconds = 0 && (years <> 0 || months <> 0 || days <> 0 || hours <> 0
			  || minutes <> 0) then "" else
	  Printf.sprintf "%d %s " seconds
	     (match seconds with
		 | 1 -> "segundo"
		 | _ -> "segundos"))

let float_seconds_proc cause seconds =
   Printf.sprintf "%.3g segundos" seconds

open Lang

let _ =
   langtime :=
      LangTime.add "es" 
	 {expand_time = expand_time_proc; 
	  float_seconds = float_seconds_proc}
	 !langtime
