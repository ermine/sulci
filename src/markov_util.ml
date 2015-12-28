open Sqlite_util
open Sqlite

let db =
   let dbf = Sqlite.db_open "./markov.db" in
      if not (result_bool dbf
	"SELECT name FROM SQLITE_MASTER WHERE type='table' AND name='words'")
      then
	 (try exec dbf 
     "CREATE TABLE words (word1 varchar(256), word2 varchar(256), counter int);"
	  with Sqlite_error s -> 
	     raise (Failure "error while creating table"));
      dbf

let add words =
   let rec cycle w1 lst =
      match lst with
	 | [] ->
	      let cond = ("word1=" ^ escape w1 ^ " AND word2=''") in 
		 if result_bool db
		    ("SELECT counter FROM words WHERE " ^ cond) then
		       exec db 
			  ("UPDATE words SET counter=counter+1 WHERE " ^ cond)
		 else
		    exec db ("INSERT INTO words VALUES(" ^ 
			     escape w1 ^ ",'',1)");
	 | w2 :: tail ->
	      if w1 = w2 then
		 cycle w2 tail
	      else begin
		 let cond = ("word1=" ^ escape w1 ^ " AND word2=" ^ 
				escape w2) in
		    if result_bool db
		       ("SELECT counter FROM words WHERE " ^ cond) then
			  exec db 
			    ("UPDATE words SET counter=counter+1 WHERE " ^ cond)
		    else
		       exec db ("INSERT INTO words VALUES(" ^ 
				   escape w1 ^ "," ^ escape w2 ^ ",1)");
		    cycle w2 tail
	      end
   in
      cycle "" words

let split_words body =
   Pcre.split ~pat:"[ \t\n]+" body

let _ =
   let file = Sys.argv.(1) in
   let fin = open_in file in
   let () = 
      try
	 while true do
	    let line = input_line fin in
	    let words = split_words line in
	       if words <> []  then
		  add words
	 done
      with End_of_file -> ()
   in
      db_close db
