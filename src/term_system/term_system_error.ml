(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierre Talbot
*)

type info = String

exception TermSystemError of Error_code.error_code * info
