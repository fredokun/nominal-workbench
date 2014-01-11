(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Matthieu Dien
*)

open Hashtbl
open Sys
open Filename
open Str
open List

exception Found of string
exception Not_Found

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

(* Give an absolute path within include_paths *)
let get_absolute_path name =
  if Filename.is_relative name then
    try
      List.iter
	(fun path ->
	  let abspath = relative_to_absolute_name (path ^ "/" ^ name) in
	  if Sys.file_exists abspath then
	    raise (Found abspath)
	  else if Sys.file_exists (abspath ^ ".nw") then
	    raise (Found (abspath ^ ".nw"))
	)
	!include_paths;
      raise Not_Found
    with
    | Found(abspath) -> abspath
    | Not_Found -> failwith ("Error : '" ^ name ^ "' doesn't exit")
  else
    let absname = (relative_to_absolute_name name) in
    if Sys.file_exists absname then
      absname
    else if Sys.file_exists (absname ^ ".nw") then
      (absname ^ ".nw")
    else
      failwith ("Error : '" ^ name ^ "' doesn't exit")

let add_file filename =
  let bname = Filename.basename filename in
  if Hashtbl.mem files_included bname then
    (* For the moment, we don't allow include of two files with same names *)
    failwith ("Error : a file named '" ^ bname ^ "' is already included")
  else
    if Sys.is_directory filename then
      failwith("Error : '" ^ filename ^ "' is not a file")
    else
      Hashtbl.add files_included bname filename

let add_path pathname =
  if not (List.mem pathname !include_paths) then
    if (Sys.file_exists pathname) && (Sys.is_directory pathname) then
      include_paths := [pathname] @ !include_paths
    else
      failwith ("Error : '" ^ pathname ^ "' is not a valid path name")
  else
    ()

let nw_include name =
  let fname = (get_absolute_path name) in
  if Filename.check_suffix fname ".nw" then
    begin
      add_file fname;
      Some(fname)
    end
  else
    begin
      add_path fname;
      None
    end
