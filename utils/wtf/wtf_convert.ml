open Dbm
open Sqlite_util
open Sqlite

let _ =
   if Array.length Sys.argv <> 4 then begin
      Printf.printf "Usage: %s dbname owner@domain\n newdb" Sys.argv.(0);
      Pervasives.exit 127
   end

let voc = opendbm Sys.argv.(1) [Dbm_rdonly] 0o666
let owner = Sys.argv.(2)

let db =
   let file = Sys.argv.(3) in

   let db = Sqlite.db_open file in
      if not (result_bool db
         "SELECT name FROM SQLITE_MASTER WHERE type='table' AND name='wtf'")
      then begin try
         exec db
            "CREATE TABLE wtf (stamp integer, nick varchar, luser varchar, lserver varchar, key varchar, value varchar)";
         exec db "create index dfnidx on wtf(key)";
      with Sqlite_error s -> 
         raise (Failure "error while creating table")
      end;
      db


let _ =
   let stamp = Int32.to_string (Int32.of_float (Unix.gettimeofday ())) in
   let at = String.index owner '@' in
   let nick = String.sub owner 0 at in
   let luser = nick in
   let lserver = String.sub owner (at+1) (String.length owner - (at+1)) in

      Dbm.iter (fun key value -> 
		   exec db 
		  ("INSERT INTO wtf (stamp, nick, luser, lserver, key, value) "
		   ^ values [stamp;
			     escape nick; escape luser; escape lserver;
			     escape key; escape value])) voc;
      print_endline "Done"
