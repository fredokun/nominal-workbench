(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Matthieu Dien
*)

(* Behaviour :
   -
*)

open Hashtbl
open Sys
open Filename
open Str
open List

let files_included = Hashtbl.create 10
let include_paths = ref [(Sys.getcwd ())]

let relative_to_absolute_name name =
  let path_list = Str.split (Str.regexp "/") name in
  let rec reduce_path_list pl =
    match pl with
      | [] -> []
      | ".." :: l -> List.tl (reduce_path_list l)
      | "." :: l -> reduce_path_list l
      | n :: l -> n :: (reduce_path_list l)
  in
  let to_concat = reduce_path_list (List.rev path_list) in
  "/" ^ (String.concat "/" (List.rev to_concat))

let get_absolute_path name =
  let absname =
    if Filename.is_relative name then
      let curr_dir = Sys.getcwd () in
      relative_to_absolute_name (curr_dir ^ "/" ^ (Filename.basename name))
    else
      name
  in
  if Sys.file_exists absname then
    absname
  else
    failwith ("Error : the file or directory '" ^ name ^ "' doesn't exist")

let add_file filename =
  let bname = Filename.basename filename in
  if Hashtbl.mem files_included bname then
    (* For the moment, we don't allow include of two files with same names *)
    failwith ("Error : a file named '" ^ bname ^ "' is already included")
  else
    if not (Filename.is_relative filename) then
      let absname = get_absolute_path filename in
      if Sys.file_exists absname then
	Hashtbl.add files_included bname absname
      else
	let absname = List.fold_left
	  (fun s p ->
	    let abspath =  (p ^ "/" ^ filename) in
	    if Sys.file_exists abspath
	    then abspath
	    else s)
	  ""
	  (!include_paths)
	  in
	  if absname = "" then
	    failwith ("Error : file '" ^ filename ^ "' not found")
	  else
	    if Sys.is_directory absname then
	      failwith ("Error : '" ^ filename ^ "' is a directory")
	    else
	      Hashtbl.add files_included bname absname

(* let add_path pathname = *)
