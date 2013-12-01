let _ =
  let chan =
    if Array.length Sys.argv > 1 then
      open_in (Sys.argv.(1))
    else
      stdin
  in
  let lexbuf = Lexing.from_channel chan in
  Parser.start Lexer.token lexbuf;
  close_in chan;
