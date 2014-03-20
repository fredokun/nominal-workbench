(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierre Talbot
*)

open Printf
open Property
open String

let make_error_types properties error_file =
  let make_error_type (name,_) =
    fprintf error_file "  | %s\n" name in
  fprintf error_file "type error_code =\n";
  List.iter make_error_type properties

let make_error_map_name properties error_file =
  let map_name (name,_) =
    fprintf error_file "  | %s -> \"%s\"\n" name (String.escaped name) in
  fprintf error_file "let string_of_error_code = function\n";
  List.iter map_name properties

let make_description_map properties error_file =
  let make_description (name, desc) =
    fprintf error_file "  | %s -> \"%s\"\n" name (String.escaped desc) in
  fprintf error_file "let description_of_error_code = function\n";
  List.iter make_description properties

(* Example: data/term_system_error.conf -> TermSystemError *)
let name_of_exception_from_filepath filepath =
  let open Str in
  let filename = replace_first (regexp ".*/\\([a-zA-Z_]+\\).conf") "\\1" filepath in
  concat "" (List.map capitalize (split (regexp "_") filename))

let make_exception error_conf_filename error_file =
  fprintf error_file "exception %s of error_code * string\n\n"
    (name_of_exception_from_filepath error_conf_filename)

let make_exception_msg error_file =
  fprintf error_file "let error_msg code info =\n";
  fprintf error_file "  Printf.sprintf (Scanf.format_from_string (description_of_error_code code) \"%%s\") info\n\n"

let make_domain_name error_conf_filename error_file =
  fprintf error_file "let domain_name = \"%s\"\n"
    (name_of_exception_from_filepath error_conf_filename)

let make_error_code error_conf error_filename =
  let error_file = open_out error_filename in
  let properties = read_property_file error_conf in
  let make_construct f = 
    f properties error_file;
    fprintf error_file "\n" in
  let construct_makers = 
    [ make_error_types; 
      make_error_map_name; 
      make_description_map ] in
  List.iter make_construct construct_makers;
  make_exception error_conf error_file;
  make_exception_msg error_file;
  make_domain_name error_conf error_file;
  close_out error_file

let ml_from_conf filename =
  let open Str in
  let filename_no_ext = replace_first (regexp "\\([a-zA-Z_]+\\).conf") "\\1" filename in
  filename_no_ext ^ ".ml"

let make_errors error_dir output_dir = 
  if not(Sys.is_directory error_dir) then
    printf "%s is not a directory.\n" error_dir
  else
    let error_files = Sys.readdir error_dir in
    Array.iter 
      (fun f -> make_error_code (error_dir ^ f) (output_dir ^ (ml_from_conf f))) 
      error_files

let usage = "Usage: error_gen <options>\n"

let main () =
  let error_conf_dir = ref "" in
  let output_dir = ref "" in
  let set_error_conf_dir d = error_conf_dir := d in
  let set_output_dir d = output_dir := d in
  let options_list =
    [ ("-d", Arg.String (set_error_conf_dir), "Error configuration directory.")
    ; ("-o", Arg.String (set_output_dir), "Output directory for the generated files.")
    ] in
  begin
    Arg.parse options_list print_endline usage;
    make_errors !error_conf_dir !output_dir
  end

let () = main ()
