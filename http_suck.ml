open Http_client

exception ClientError
exception ServerError
exception Redirect
exception Sys
exception Unserved

exception HTTP_Job of http_call * (http_call -> unit)

let http_esys = ref None

let get_http_esys() =
  match !http_esys with
    | None -> failwith "No event system"
    | Some e -> e

let http_keep_alive_group = ref None 

let get_http_keep_alive_group() =
  match !http_keep_alive_group with
    | None -> failwith "No keep alive group"
    | Some g -> g

let http_init() =
  let esys = Unixqueue.create_unix_event_system() in
  let keep_alive_group = Unixqueue.new_group esys in
    http_esys := Some esys;
    http_keep_alive_group := Some keep_alive_group
      
let http_thread() =
  let esys = get_http_esys() in
  let pipeline = new pipeline in
    pipeline # set_event_system esys;
    let keep_alive_group = get_http_keep_alive_group() in
    let w = Unixqueue.new_wait_id esys in
      Unixqueue.add_resource esys keep_alive_group (Unixqueue.Wait w,(-1.0));
      Unixqueue.add_handler
        esys
        keep_alive_group
        (fun _ _ event ->
           match event with
             | Unixqueue.Extra (HTTP_Job (call, f_done)) ->
                 (try
                    pipeline # add_with_callback call f_done
                  with exn ->
                    f_done call
                 )
             | _ ->
                 raise Equeue.Reject  (* The event is not for us *)
        );
      Unixqueue.run esys

let shutdown_http_thread() =
  let esys = get_http_esys() in
  let keep_alive_group = get_http_keep_alive_group() in
    Unixqueue.clear esys keep_alive_group;
    http_keep_alive_group := None;
    http_esys := None
      
type result =
  | OK of string option * string option * string
  | Exception of exn

let get_media_type call =
  let headers = call # response_header in
    try
      let media_type, params = headers # content_type () in
      let charset = 
        try let value = List.assoc "charset" params in
          Some (Mimestring.param_value value)
        with Not_found -> None in
        Some media_type, charset
    with Not_found ->
      None, None
        
let request call callback =
  let f_done call =
    let result = match call # status with
      | `Successful ->
          let media, charset = get_media_type call in
          let content = call # response_body # value in
            OK (media, charset, content)
      | `Client_error ->
          Exception ClientError
      | `Server_error ->
          Exception ServerError
      | `Http_protocol_error exn ->
          Exception exn
      | `Redirection ->    (* TODO *)
          Exception Redirect
      | `Unserved ->       (* raises at add_with_callback *)
          Exception ClientError
    in
      callback result
  in
  let esys = get_http_esys() in
    Unixqueue.add_event esys (Unixqueue.Extra (HTTP_Job (call, f_done)))
      
let http_get url callback =
  request (new get url) callback
    
let http_post url headers data callback =
  let p = new post_call in
  let h = p # request_header `Base in
    p # set_request_uri url;
    List.iter (fun (key, value) -> h # update_field key value) headers;
    let b = new Netmime.memory_mime_body data in
      p # set_request_body b;
      h # update_field "Content-length" (string_of_int (String.length data));
      request p callback
        
let _ =
  (* Unixqueue.set_debug_mode true; *)
  http_init();
  let _http_thr = Thread.create http_thread () in
    ()
