open Common

let pcalc text xml out =
   if text <> "" then
      let reply = 
	 try
            let lexbuf = Lexing.from_string text in
               Pcalc.line Pcalc_lexer.token lexbuf
	 with exn ->
	    Lang.get_msg ~xml "plugin_calc_not_parsed" []
      in
	 out (make_msg xml reply)
   else
      out (make_msg xml (Lang.get_msg ~xml "plugin_calc_empty_command" []))

let icalc text xml out =
   if text <> "" then
      let reply = 
	 try
            let lexbuf = Lexing.from_string text in
               Icalc.line Icalc_lexer.token lexbuf
	 with 
	    | Failure err ->
		 err
	    | exn ->
		 Lang.get_msg ~xml "plugin_calc_not_parsed" []
      in
	 out (make_msg xml reply)
   else
      out (make_msg xml (Lang.get_msg ~xml "plugin_calc_empty_command" []))

let _ =
   Hooks.register_handle (Hooks.Command ("rpn", pcalc));
   Hooks.register_handle (Hooks.Command ("calc", icalc))
