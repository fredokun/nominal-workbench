open String
open Printf

let rec iter_lines f channel = 
  try
    f (input_line channel)
  with End_of_file -> ()

let iter_file_lines f filename = 
  let file_channel = open_in filename in
  try
    iter_lines f file_channel;
    close_in file_channel
  with
  | e ->
      close_in_noerr file_channel;
      raise e

let read_property line =
  let open Str in
  let sep_index = index line ':' in
  let property f = trim (f line sep_index) in
  ((property string_before), (property string_after))

let make_error_types error_conf error_file =
  let make_error_type line =
    let (error_code_name, _) = read_property line in
    fprintf error_file "  | %s\n" error_code_name in
  begin
    fprintf error_file "type error_code =\n";
    iter_file_lines make_error_type error_conf
  end

let make_error_code error_conf error_ml =
  let error_file = open_out error_ml in
  make_error_types error_conf error_file;
  close_out error_file

let main () =
  make_error_code "data/error.conf" "src/generated/error_code.ml"

let () = main ()
