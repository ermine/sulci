(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Conversion
open Hooks

let unknown_encoding_handler encoding =
  let decoder = Conversion.make_decoder encoding in
    fun str i ->
      match decoder str i with
        | Cs.Shift j -> Xmlencoding.Shift j
        | Cs.Invalid -> Xmlencoding.Invalid
        | Cs.TooFew -> Xmlencoding.TooFew
        | Cs.Result (j, ucs4) -> Xmlencoding.Result (j, ucs4)

let parse_document  content =
  Light_xml.parse_document ~unknown_encoding_handler content

let plugin opts =
  let decoder_dir = List.assoc "path" (List.assoc "decoder_dir" opts) in
  let encoder_dir = List.assoc "path" (List.assoc "encoder_dir" opts) in
    Conversion.init ~decoder_dir ~encoder_dir ()

let _ =
  add_plugin "conversion" plugin
