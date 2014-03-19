(* Distributed under the MIT License.
   (See accompanying file LICENSE.txt)
   (C) Copyright Vincent Botbol
*)

let () = Main.main
  (fun system -> Toploop.loop Format.std_formatter system)
