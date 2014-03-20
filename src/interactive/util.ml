(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Matthieu Dien
*)

let parse_channel chan =
  let lexbuf = Lexing.from_channel chan in
  let res = Parser.start Lexer.token lexbuf in
  close_in chan;
  res

let list_foldmap f init l =
  let rec aux (ys, accu) = function
    | [] ->
      (List.rev ys, accu)
    | x :: xs ->
      let (y, accu) = f accu x in
      aux (y :: ys, accu) xs
  in
  aux ([], init) l

