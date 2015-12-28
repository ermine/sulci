(*
 * (c) 2005-2012 Anastasia Gornostaeva
 *)

open Hooks
open Plugin_command

module VCard = XEP_vcard.Make(XMPPClient)
open VCard

let result_vcard vcard =
  let nick, desc, email, bday, url =
    List.fold_left (fun (nick, desc, email, bday, url) -> function
      | Nickname nick ->
        (nick, desc, email, bday, url)
      | Desc desc ->
        (nick, desc, email, bday, url)
      | URL url ->
        (nick, desc, email, bday, url)
      | BDay bday ->
        (nick, desc, email, bday, url)
      | Email email ->
        (nick, desc, email.userid, bday, url)
      | _ ->
        (nick, desc, email, bday, url)
    ) ("", "", "", "", "") vcard.fields
  in
    String.concat "--" [vcard.fn; nick; email; url; desc; bday]
      
let vsearch =
  let success _env _text _entity = function
    | None -> "no info"
    | Some el -> result_vcard (VCard.decode el)
  in
    Iq.simple_query_entity success ~payload:(VCard.make_iq_get ())
      
let plugin opts =
  add_for_token
    (fun _opts xmpp ->
      add_commands xmpp [("vcard", vsearch)] opts
    )
    
let _ =
  Plugin.add_plugin "vcard" plugin
    
