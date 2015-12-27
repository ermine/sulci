open Unix

open Scheduler

let _ =
  let f time i () =
    let tm1 = localtime (gettimeofday ()) in
    let tm2 = localtime time in
	    Printf.printf "%02d:%02d msg %d -- %02d:%02d\n"
	      tm1.tm_min tm1.tm_sec i tm2.tm_min tm2.tm_sec;
	    flush Pervasives.stdout
  in
  let timerQ = create () in
  let t = run timerQ in
  let () = Random.self_init () in
    for i = 1 to 100 do
	    let time = (gettimeofday ()) +. Random.float 180. in
	      if i mod 10 = 0 then begin
	        flush Pervasives.stdout;
	        sleep 1
	      end;
	      let r = add_task timerQ (f time i) time
	        (fun() -> time +. Random.float 1800.) in
	      let _ = add_task timerQ (fun () -> 
				  Printf.printf "%d cancelled\n" i;
				  flush Pervasives.stdout;
				  r.cancelled <- true)
	        (time +. Random.float 1000.) (fun () -> 0.0)
	      in ()
    done;
    Thread.join t
