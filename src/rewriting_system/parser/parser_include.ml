(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Matthieu Dien
*)

open Lexing
open Parser
open Include
open Lexer

let rec parse_channel channel =
  let lexbuf = Lexing.from_channel channel in
  let res = Parser.start Lexer.token lexbuf in
  close_in channel;
  match res with
    | (decls, []) -> [decls]
    | (decls, l) -> decls :: (parse_file_list l)
and parse_file filename =
  let fchan = open_in filename in
  parse_channel fchan
and parse_file_list l =
  match l with
    | [] -> []
    | f :: q -> (parse_file f) @ parse_file_list q
