open Dbm

let _ =
  let src = Sys.argv.(1) in
  let dst = Sys.argv.(2) in
  let fin = open_in src in
  let tlds = Dbm.opendbm dst [Dbm_create; Dbm_rdwr] 0o644 in
    try
	    while true do
	      let line = input_line fin in
	      let space = String.index line ' ' in
	      let key = String.sub line 0 space in
	      let value = String.sub line (space+1) 
	        (String.length line - space - 1) in
	        Dbm.add tlds key value
	    done
    with End_of_file ->
	    close_in fin;
	    Dbm.close tlds
