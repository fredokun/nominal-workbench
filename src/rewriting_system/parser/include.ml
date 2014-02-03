(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Matthieu Dien
*)

open Hashtbl
open Sys
open Filename
open List

let files_included = Hashtbl.create 10
let include_paths = ref [(Sys.getcwd ())]

let rec split_slash str =
  try
    let pos = String.index str '/' in
    (String.sub str 0 pos)::(split_slash
			    (String.sub str (pos+1)
				    ((String.length str)-pos-1)))
  with
    | Not_found -> [str]

(* Never call it directly *)
let relative_to_absolute_name name =
  let path_list = split_slash name in
  let rec reduce_path_list pl =
    match pl with
      | [] -> []
      | ".." :: l -> List.tl (reduce_path_list l)
      | "." :: l -> reduce_path_list l
      | n :: l -> n :: (reduce_path_list l)
  in
  let to_concat = reduce_path_list (List.rev path_list) in
  "/" ^ (String.concat "/" (List.rev to_concat))

let rec find_file_in_include_paths name inc_paths =
  match inc_paths with
    | [] -> None
    | path :: t -> let abspath = relative_to_absolute_name (path ^ "/" ^ name) in
		   if Sys.file_exists abspath then
		     Some abspath
		   else if Sys.file_exists (abspath ^ ".nw") then
		     Some (abspath ^ ".nw")
		   else
		     find_file_in_include_paths name t

(* Give an absolute path within include_paths *)
let get_absolute_path name =
  if Filename.is_relative name then
    match (find_file_in_include_paths name !include_paths) with
      | Some(abspath) -> abspath
      | None -> failwith ("Error : '" ^ name ^ "' doesn't exit")
  else
    let absname = (relative_to_absolute_name name) in
    if Sys.file_exists absname then
      absname
    else if Sys.file_exists (absname ^ ".nw") then
      (absname ^ ".nw")
    else
      failwith ("Error : '" ^ name ^ "' doesn't exit")

(* add a file to the includes tables *)
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

(* add a path to the includes tables *)
let add_path pathname =
  if not (List.mem pathname !include_paths) then
    if (Sys.file_exists pathname) && (Sys.is_directory pathname) then
      include_paths := [pathname] @ !include_paths
    else
      failwith ("Error : '" ^ pathname ^ "' is not a valid path name")
  else
    ()

(* give it a string, it do the job *)
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
