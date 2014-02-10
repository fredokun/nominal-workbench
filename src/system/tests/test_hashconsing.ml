open Term_ast_dag
open Term_ast_hashconsed

let create_tests () =
  (* Testing for Const *)

  let c1 = DConst "A" in
  let c2 = DConst "B" in
  let c3 = DConst "B" in

  let hc1 = create_term c1 in
  let hc2 = create_term c2 in
  let hc3 = create_term c3 in

  assert (not (hc1 == hc2));
  assert (hc2 == hc3);

  (* Testing for FreeVars *)

  let fv1 = DVar ("v", None) in
  let fv2 = DVar ("u", None) in
  let fv3 = DVar ("v", None) in

  let hfv1 = create_term fv1 in
  let hfv2 = create_term fv2 in
  let hfv3 = create_term fv3 in

  assert (not (hfv1 == hfv2));
  assert (hfv1 == hfv3);

  (* Testing for Vars *)

  let b1 = DBinder ("a", ref []) in
  let b2 = DBinder ("b", ref []) in
  let v1 = DVar ("a", Some (ref b1)) in
  let v2 = DVar ("b", Some (ref b2)) in
  let v3 = DVar ("b", Some (ref b2)) in

  (* We must bind the binders in order to have a correct index *)
  let hb1, n, bindings = create_term_raw 0 SMap.empty b1 in
  let hb2, n, bindings = create_term_raw n bindings b2 in

  let hv1, n, bindings = create_term_raw n bindings v1 in
  let hv2, n, bindings = create_term_raw n bindings v2 in
  let hv3, n, bindings = create_term_raw n bindings v3 in

  assert (not (hv1 == hv2));
  assert (hv2 == hv3);

  (* Term_list tests *)
  (* We create hlists using previously defined terms (which can be correctly
    hashconsed) *)

  let l1 = [v1; v2] in
  let l2 = [v1; v2] in

  let hl1 = create_hlist n bindings l1 in
  let hl2 = create_hlist n bindings l2 in

  assert(hl1 == hl2);

  (* We now test term creation, since other constructs and lists can be
     correctly hashconsed. *)
  (* We suppose a system with constants A and B which are Naturals and addition
     on it.
     Two operators :
     - Bind : [Variable] * Nat -> Nat
     - Add : Nat * Nat -> Nat
  *)

  let add1 = DTerm ("Add", [v1; c1]) in
  let t1 = DTerm ("Bind", [b1; add1]) in

  let add2 = DTerm ("Add", [v2; c1]) in
  let t2 = DTerm ("Bind", [b2; add2]) in

  let ht1 = create_term t1 in
  let ht2 = create_term t2 in

  assert (ht1 == ht2)

let peano_tests () =
  (* Tests with more complex terms *)

  (* We now suppose the Peano system
     Only one constant : Zero, and two operators : Successor and Add.
     There is no binders in this system.
  *)

  let zero = DConst "Zero" in
  let one = DTerm ("Successor", [zero]) in
  let one2 = DTerm ("Successor", [zero]) in

  let add1 = DTerm ("Add", [one; one]) in
  let add2 = DTerm ("Add", [one; one2]) in
  let add3 = DTerm ("Add", [one2; one]) in
  let add4 = DTerm ("Add", [one2; one2]) in

  let ha1 = create_term add1 in
  let ha2 = create_term add2 in
  let ha3 = create_term add3 in
  let ha4 = create_term add4 in

  assert(ha1 == ha2);
  assert(ha2 == ha3);
  assert(ha3 == ha4)

let lambda_tests () =
  (* The lambda calculus uses binders, it can be more tricky.
     There is no constant, but variables (free or not).
     We have four operators :
     - Var : Variable -> Term
     - App : Term * Term -> Term
     - Lambda : [Variable]. Term -> Term
     - Subst : [Variable]. Term * Term -> Term
  *)

  (* First, we try on two identical id functions, modulo apha-conversion *)
  let bx = DBinder ("x", ref []) in
  let id1 = DTerm ("Lambda", [bx; DVar ("x", Some (ref bx))]) in
  let by = DBinder ("y", ref []) in
  let id2 = DTerm ("Lambda", [by; DVar ("y", Some (ref by))]) in

  let hid1 = create_term id1 in
  let hid2 = create_term id2 in

  assert (hid1 == hid2)

let _ =
  create_tests ();
  peano_tests ();
  lambda_tests ()
