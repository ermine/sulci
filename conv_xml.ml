(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

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
