(*
 * (c) 2004-2011 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Text
open Hooks

let unknown_encoding_handler encoding =
  let decoder = Encoding.decoder encoding in
    fun str i ->
      match Encoding.decode decoder str i (String.length str - i) with
        | Encoding.Dec_ok (ucs4, j) -> Xmlencoding.Result ((i+j), ucs4)
        | Encoding.Dec_need_more -> Xmlencoding.TooFew
        | Encoding.Dec_error -> Xmlencoding.Invalid
            
let parse_document  content =
  Light_xml.parse_document ~unknown_encoding_handler content

let plugin _opts =
  ()

let _ =
  Plugin.add_plugin "conversion" plugin
