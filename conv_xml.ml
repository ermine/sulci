(*
 * (c) 2004-2012 Anastasia Gornostaeva
 *)

open Hooks

module StringStream =
struct
  type 'a t = 'a
  let return x = x
  let fail = raise
    
  type stream = {
    mutable i : int;
    len : int;
    buf : string
  }

  let of_string str =
    { buf = str; len = String.length str; i = 0}

  let get s =
    if s.i < s.len then (
      let c = s.buf.[s.i] in
        s.i <- s.i + 1;
        Some c
    )
    else
      None

  open Encoding
  exception IllegalCharacter
    
  let make_decoder encname =
    let decoder = decoder encname in
      fun strm ->
        if strm.i < strm.len then
          match decode decoder strm.buf strm.i (strm.len - strm.i) with
            | Dec_ok (ucs4, j) ->
              strm.i <- strm.i + j;
              return (Some ucs4)
            | Dec_need_more -> fail IllegalCharacter
            | Dec_error -> fail IllegalCharacter
        else
          return None
end

open Xmllexer

module LS = LocatedStream (UnitMonad) (StringStream)
module M = Xmllexer_generic.Make
  (LS)
  (Encoding)
  (XmlStanza (UnitMonad))

let parse_document  content =
  let strm = StringStream.of_string content in
  let strm = LS.make_stream strm in
  let next_token = M.make_lexer strm in
    Light_xml.parse next_token

let plugin _opts =
  ()

let _ =
  Plugin.add_plugin "conversion" plugin
