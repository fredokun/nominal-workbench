open Printf
open Property

let make_error_types properties error_file =
  let make_error_type (name,_) =
    fprintf error_file "  | %s\n" name in
  fprintf error_file "type error_code =\n";
  List.iter make_error_type properties

let make_error_map_name properties error_file =
  let map_name (name,_) =
    fprintf error_file "  | %s -> \"%s\"\n" name name in
  fprintf error_file "let string_of_error_code = function\n";
  List.iter map_name properties

let make_description_map properties error_file =
  let make_description (name, desc) =
    fprintf error_file "  | %s -> \"%s\"\n" name desc in
  fprintf error_file "let description_of_error_code = function\n";
  List.iter make_description properties

let make_error_code error_conf error_filename =
  let error_file = open_out error_filename in
  let properties = read_property_file error_conf in
  make_error_types properties error_file;
  fprintf error_file "\n";
  make_error_map_name properties error_file;
  fprintf error_file "\n";
  make_description_map properties error_file;
  close_out error_file

let main () =
  make_error_code "data/error.conf" "src/auto_gen/error_code.ml"

let () = main ()
