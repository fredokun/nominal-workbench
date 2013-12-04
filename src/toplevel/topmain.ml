
let usage = "Usage: nowork <options> files\noptions are:"

let file_argument name =
  ignore name


let print_version () =
  Printf.printf "NoWork toplevel, version %s\n" "0.00.1" (* Config.version *);
  exit 0

let print_vnum () =
  Printf.printf "%s\n" "0.00.1" (* Config.version *);
  exit 0


let options_list = 
  let open Arg in
      [ "version", Unit print_version, "Print version and exits"
      ; "vnum", Unit print_vnum, "Print version number and exits"
      ]

let main () =
  begin
    Arg.parse options_list file_argument usage;
    Toploop.loop Format.std_formatter
  end

let () = main ()
