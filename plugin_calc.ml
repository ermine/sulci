(*
 * (c) 2004-2010 Anastasia Gornostaeva
 *)

open Common
open Hooks
open Plugin_command
open Math
open Icalc

let pcalc xmpp env kind jid_from text =
  if text <> "" then
    let reply = 
      try
        let lexbuf = Lexing.from_string text in
          Pcalc.line Pcalc_lexer.token lexbuf
      with 
        | MathNumberTooBig ->
            Lang.get_msg env.env_lang "plugin_calc_number_too_big" []
        | MathCannotFloatFact ->
            Lang.get_msg env.env_lang "plugin_calc_cannot_float_fact" []
        | MathNegNumber ->
            Lang.get_msg env.env_lang "plugin_calc_negative_number" []
        | _ ->
            Lang.get_msg env.env_lang "plugin_calc_not_parsed" []
    in
      env.env_message xmpp kind jid_from reply
  else
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_calc_empty_command" [])
      
let icalc xmpp env kind jid_from text =
  if text <> "" then
    let reply = 
      try
        let lexbuf = Ulexing.from_utf8_string text in
          Icalc.line (fun _ -> Icalc_ulex.token lexbuf) 
            (Lexing.from_string "dummy")
            (*
              let lexbuf = Lexing.from_string text in
              Icalc.line Icalc_lexer.token lexbuf
            *)
      with 
        | MathNumberTooBig ->
            Lang.get_msg env.env_lang "plugin_calc_number_too_big" []
        | MathCannotFloatFact ->
            Lang.get_msg env.env_lang "plugin_calc_cannot_float_fact" []
        | MathNegNumber ->
            Lang.get_msg env.env_lang "plugin_calc_negative_number" []
        | Failure _err ->
            Lang.get_msg env.env_lang "plugin_calc_divide_by_zero" []
        | _exn ->
            Lang.get_msg env.env_lang "plugin_calc_not_parsed" []
    in
      env.env_message xmpp kind jid_from reply
  else
    env.env_message xmpp kind jid_from
      (Lang.get_msg env.env_lang "plugin_calc_empty_command" [])
      
let plugin opts =
  add_for_token
    (fun _opts xmpp ->
       add_commands xmpp [("calc", icalc); ("rpn", pcalc)] opts
    )

let _ =
  Plugin.add_plugin "calc" plugin
