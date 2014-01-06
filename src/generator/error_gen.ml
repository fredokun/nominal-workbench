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

let make_error_code error_conf error_filename =
  let error_file = open_out error_filename in
  let properties = read_property_file error_conf in
  let make_construct f = 
    f properties error_file;
    fprintf error_file "\n" in
  let construct_makers = 
    [ make_error_types; 
      make_error_map_name; 
      make_description_map] in
  List.iter make_construct construct_makers;
  close_out error_file

let usage = "Usage: error_gen <options>\n"

let main () =
  let error_conf = ref "" in
  let output_file = ref "" in
  let set_error_conf f = error_conf := f in
  let set_output_file f = output_file := f in
  let options_list =
    [ ("-c", Arg.String (set_error_conf), "Error configuration file.")
    ; ("-o", Arg.String (set_output_file), "OCaml output file.")
    ] in
  begin
    Arg.parse options_list print_endline usage;
    make_error_code !error_conf !output_file
  end

let () = main ()
