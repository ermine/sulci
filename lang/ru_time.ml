let expand_time_proc cause years months days hours minutes seconds =
   (if years = 0 then "" else 
       Printf.sprintf "%d %s " years
	  (match years with
	      | x when x >=5 && x <= 20 -> "лет"
	      | x when x mod 10 = 1 -> "год"
	      | x when x mod 10 = 2 -> "года"
	      | x when x mod 10 = 3 -> "года"
	      | x when x mod 10 = 4 -> "года"
	      | _ -> "лет")) ^
      (if months = 0 then "" else
	  Printf.sprintf "%d %s " months
	     (match months with
		 | x when x >=5 && x <= 20 -> "месяцев"
		 | x when x mod 10 = 1 -> "месяц"
		 | x when x mod 10 = 2 -> "месяца"
		 | x when x mod 10 = 3 -> "месяца"
		 | x when x mod 10 = 4 -> "месяца"
		 | _ -> "месяцев")) ^
      (if days = 0 then "" else
	  Printf.sprintf "%d %s " days
	     (match days with
		 | x when x >=5 && x <= 20 -> "дней"
		 | x when x mod 10 = 1 -> "день"
		 | x when x mod 10 = 2 -> "дня"
		 | x when x mod 10 = 3 -> "дня"
		 | x when x mod 10 = 4 -> "дня"
		 | _ -> "дней")) ^
      (if hours = 0 then "" else
	  Printf.sprintf "%d %s " hours
	     (match hours with
		 | x when x >= 5 && x <= 20 -> "часов"
		 | x when x mod 10 = 1 -> "час"
		 | x when List.mem (x mod 10) [2;3;4] -> "часа"
		 | _ -> "часов")) ^
      (if minutes = 0 then "" else
	  Printf.sprintf "%d %s " minutes
	     (match minutes with
		 | x when x >=5 && x <= 20 -> "минут"
		 | x when x mod 10 = 1 -> (match cause with 
					      | "seen"
					      | "idle"
					      | "uptime"
					      | _ -> "минуту")
		 | x when x mod 10 = 2 -> "минуты"
		 | x when x mod 10 = 3 -> "минуты"
		 | x when x mod 10 = 4 -> "минуты"
		 | x -> "минут")) ^
      (if seconds = 0 && (years <> 0 || months <> 0 || days <> 0 || hours <> 0
			  || minutes <> 0) then "" else
	  Printf.sprintf "%d %s " seconds
	     (match seconds with
		 | x when x >=5 && x <= 20 -> "секунд"
		 | x when x mod 10 = 1 -> "секунду"
		 | x when x mod 10 = 2 -> "секунды"
		 | x when x mod 10 = 3 -> "секунды"
		 | x when x mod 10 = 4 -> "секунды"
		 | _ -> "секунд"))

let float_seconds_proc cause seconds =
   Printf.sprintf "%.3g секунды" seconds

open Lang

let _ =
   langtime := 
      LangTime.add "ru" 
	 {expand_time = expand_time_proc; 
	  float_seconds = float_seconds_proc}
	 !langtime
