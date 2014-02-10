open Term_ast_dag
open Term_ast_hashconsed

let _ =
  (* Testing for Const *)

  let c1 = DConst "A" in
  let c2 = DConst "B" in
  let c3 = DConst "B" in

  let hc1 = create_term c1 in
  let hc2 = create_term c2 in
  let hc3 = create_term c3 in

  pretty_print hc1;
  pretty_print hc2;
  pretty_print hc3;

  assert (not (hc1 == hc2));
  assert (hc2 == hc3)
