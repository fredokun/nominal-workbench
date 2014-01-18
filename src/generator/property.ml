(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierre Talbot
*)

open String
open Printf

let rec map_lines f channel = 
  try
    let mapping = f (input_line channel) in
    mapping :: (map_lines f channel)
  with End_of_file -> []

let map_file_lines f filename = 
  let file_channel = open_in filename in
  try
    let mapping = map_lines f file_channel in
      close_in file_channel;
      mapping
  with e ->
    close_in_noerr file_channel;
    raise e

let read_property line =
  let open Str in
  let sep_index = index line ':' in
  let property f idx = trim (f line idx) in
  ((property string_before sep_index), (property string_after (sep_index+1)))

let read_property_file filename = 
  map_file_lines read_property filename