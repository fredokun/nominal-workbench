(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierre Talbot
*)

type info = String

exception TermSystemError of Term_system_error_code.error_code * info
