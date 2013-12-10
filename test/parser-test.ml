let parse_chan = fun chan ->
  let lexbuf = Lexing.from_channel chan in
  let res = Parser.start Lexer.token lexbuf in
  close_in chan;
  res

let _ =
  if Array.length Sys.argv > 1 then
    begin
      print_endline (Sys.argv.(1));
      let arg1 = Sys.argv.(1) in
      if Sys.file_exists arg1 then
	begin
	  if Sys.is_directory arg1 then 
	    let test_files = Sys.readdir arg1 in
	    let res = ref 0 in
	    Array.iter
	      (fun x -> res := !res + (parse_chan (open_in (arg1^"/"^x))))
	      test_files;
	    (if !res = 0 then exit 0 else exit 1)
	  else
	    exit (parse_chan (open_in (arg1)))
	end
      else
	prerr_endline "unknown file or folder";
      exit 1
    end
  else
    ignore (parse_chan stdin)
