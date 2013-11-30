let _ =
  let filename = Sys.argv.(1) in
  let file_p = open_in filename in
  (* let l = in_channel_length file_p in *)
  (* let s = String.create l in *)
  (* really_input file_p s 0 l; *)
  let lexbuf = Lexing.from_channel file_p in
  Parser.start Lexer.token lexbuf;
  close_in file_p;
