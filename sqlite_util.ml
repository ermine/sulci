(*
 * (c) 2008-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Sqlite3

let string_after s n =
  String.sub s n (String.length s - n)

let escape str =
  let rec aux i acc =
    try
      let q = String.index_from str i '\'' in
        aux (q+1)  (((String.sub str i q) ^ "''") :: acc)
    with Not_found ->
      string_after str i :: acc
  in
    "'" ^ (String.concat "" (List.rev (aux 0 []))) ^ "'"

let exit_with_rc file db sql =
  (match errcode db with
     | Rc.NOTADB ->
         Printf.eprintf "File %s is not a Sqlite3 database\n" file;
     | Rc.ERROR ->
         Printf.eprintf "File %s: Bad SQL:\n%s\n" file sql;
     | rc ->
         Printf.eprintf "File %s: %s\n" file (Rc.to_string rc);
  );
  flush Pervasives.stderr;
  Pervasives.exit 127

let simple_exec file db sql =
  try match exec db sql with
    | Rc.OK -> ()
    | _ -> exit_with_rc file db sql
  with Sqlite3.Error _ ->
    exit_with_rc file db sql
      
      
let create_table file db sql1 sql2 =
  let found =
    try
      let stmt = prepare db sql1 in
        match step stmt with
          | Rc.ROW -> true
          | Rc.DONE -> false
          | _ -> exit_with_rc file db sql1
    with Sqlite3.Error _ -> exit_with_rc file db sql1
  in
    if not found then
      simple_exec file db sql2
        
let get_one_row file db sql =
  try
    let stmt = prepare db sql in
    let res =
      match step stmt with
        | Rc.ROW -> Some (row_data stmt)
        | Rc.DONE -> None
        | _ -> exit_with_rc file db sql
    in
      if finalize stmt <> Rc.OK then
        exit_with_rc file db sql;
      res
  with Sqlite3.Error _ -> exit_with_rc file db sql
    
let insert_or_update file db sql1 sql2 sql3 =
  let found =
    match get_one_row file db sql1 with
      | Some _ -> true
      | None -> false
  in
    simple_exec file db (if found then sql2 else sql3);
    found
      
(*
type t =
  | NONE
  | NULL
  | INT of int64
  | FLOAT of float
  | TEXT of string
  | BLOB of string
*)

open Sqlite3.Data
      
let int64_of_data = function
  | INT i -> i
  | _ -> raise (Invalid_argument "int64_of_data")

let float_of_data = function
  | FLOAT f -> f
  | _ -> raise (Invalid_argument "float_of_data")

let string_of_data = function
  | TEXT t -> t
  | BLOB t -> t
  | _ -> raise (Invalid_argument "string_of_data")
