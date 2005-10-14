open Http_client

exception ClientError
exception ServerError
exception Redirect
exception Sys
exception Unserved

exception HTTP_Job of http_call * (http_call -> unit)

class my_pipeline =
object
   inherit pipeline as super
   method add_with_callback request f_done =
      try
         super#add_with_callback request f_done;
      with exn ->
         Logger.print_exn "http_suck.ml" exn;
         f_done request
end

class my_get the_query =
object
   inherit get the_query as super
   method set_request_uri query =
      try
         super # set_request_uri query
      with exn ->
         Logger.print_exn "http_suck.ml" exn;
         raise (Http_protocol exn)
end

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
   (* Create the HTTP pipeline for a known event system: *)
   let esys = get_http_esys() in
   let pipeline = new my_pipeline in
      pipeline # set_event_system esys;
      
     (* In order to keep the event system active when there are no HTTP requests
      * to process, we add an artificial timer that never times out (-1.0).
      * The timer is bound to a Unixqueue group, and by clearing this group
      * the timer can be deleted.
      *)
      let keep_alive_group = get_http_keep_alive_group() in
      let w = Unixqueue.new_wait_id esys in
	 Unixqueue.add_resource esys keep_alive_group (Unixqueue.Wait w,(-1.0));

	 (* We arrange now that whenever a HTTP_Job arrives on the event queue,
	  * a new HTTP call is started.
	  *)
	 Unixqueue.add_handler
	    esys
	    keep_alive_group
	    (fun _ _ event ->
		match event with
		   | Unixqueue.Extra (HTTP_Job (call, f_done)) ->
			try
			   pipeline # add_with_callback call f_done
			with exn ->
			   f_done call
		   | _ ->
			raise Equeue.Reject  (* The event is not for us *)
	    );
	 
	 (* Now start the event queue. It returns when all jobs are done and
	  * the keep_alive_group is cleared.
	  *)
	 Unixqueue.run esys;
	 ()

let shutdown_http_thread() =
   let esys = get_http_esys() in
   let keep_alive_group = get_http_keep_alive_group() in
      Unixqueue.clear esys keep_alive_group;
      http_keep_alive_group := None;
      http_esys := None

type result = | OK of string | Exception of exn

let request call callback =
   let f_done call =
      try
	 let result = match call # status with
	    | `Successful ->
		 let body = call # response_body # value in
		    body
            | `Client_error ->
		 raise ClientError
            | `Server_error ->
		 raise ServerError
            | `Http_protocol_error exn ->
		 raise exn
            | `Redirection ->    (* TODO *)
		 raise Redirect
            | `Unserved ->       (* raises at add_with_callback *)
		 raise ClientError
	 in
	    callback (OK result)
      with exn ->
	 callback (Exception exn)
   in
   let esys = get_http_esys() in
      Unixqueue.add_event esys (Unixqueue.Extra (HTTP_Job (call, f_done)))

let http_get url callback =
   request (new my_get url) callback

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
   (* Start the HTTP thread: *)
   let http_thr = Thread.create http_thread () in
      ()
