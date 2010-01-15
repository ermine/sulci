(*
 * (c) 2005-2010 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open XEP_vcard
open Hooks
open Plugin_command

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
    | Some el -> result_vcard (XEP_vcard.decode el)
  in
    Iq.simple_query_entity success ~payload:(XEP_vcard.make_iq_get ())
     
let plugin opts =
  add_for_token
    (fun _opts xmpp ->
       add_commands xmpp [("vcard", vsearch)] opts
    )

let _ =
  Plugin.add_plugin "vcard" plugin
