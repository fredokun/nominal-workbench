open Term_ast_typed
open Term_checker
open Term_ast_hashconsed

let create_tests () =
  Format.printf "Starting hashconsed terms creation tests@.";
  (* Testing for Const *)

  let c1 = DConst (None, "A") in
  let c2 = DConst (None, "B") in
  let c3 = DConst (None, "B") in

  let hc1 = create_term c1 in
  let hc2 = create_term c2 in
  let hc3 = create_term c3 in

  assert (not (hc1.term == hc2.term));
  assert (hc2.term == hc3.term);

  Format.printf "Const OK@.";

  (* Testing for FreeVars *)

  let fv1 = DVar (None, "v") in
  let fv2 = DVar (None, "u") in
  let fv3 = DVar (None, "v") in

  let hfv1 = create_term fv1 in
  let hfv2 = create_term fv2 in
  let hfv3 = create_term fv3 in

  assert (not (hfv1.term == hfv2.term));
  assert (hfv1.term == hfv3.term);

  Format.printf "FreeVars OK@.";

  (* Testing for Vars *)

  let b1 = DBinder (None, "a") in
  let b2 = DBinder (None, "b") in
  let v1 = DVar (None, "a") in
  let v2 = DVar (None, "b") in

  (* We now test term creation, since other constructs and lists can be
     correctly hashconsed. *)
  (* We suppose a system with constants A and B which are Naturals and addition
     on it.
     Two operators :
     - Bind : [Variable] * Nat -> Nat
     - Add : Nat * Nat -> Nat
  *)

  let add1 = DTerm (None, "Add", [v1; c1]) in
  let t1 = DTerm (None, "Bind", [b1; add1]) in

  let add2 = DTerm (None, "Add", [v2; c1]) in
  let t2 = DTerm (None, "Bind", [b2; add2]) in

  let ht1 = create_term t1 in
  let ht2 = create_term t2 in

  assert (ht1.term == ht2.term);
  Format.printf "Terms OK@."

let peano_tests () =
  (* Tests with more complex terms *)

  (* We now suppose the Peano system
     Only one constant : Zero, and two operators : Successor and Add.
     There is no binders in this system.
  *)

  Format.printf "Starting Peano terms hashconsing@.";

  let zero = DConst (None, "Zero") in
  let one = DTerm (None, "Successor", [zero]) in
  let one2 = DTerm (None, "Successor", [zero]) in

  let add1 = DTerm (None, "Add", [one; one]) in
  let add2 = DTerm (None, "Add", [one; one2]) in
  let add3 = DTerm (None, "Add", [one2; one]) in
  let add4 = DTerm (None, "Add", [one2; one2]) in

  let ha1 = create_term add1 in
  let ha2 = create_term add2 in
  let ha3 = create_term add3 in
  let ha4 = create_term add4 in

  assert(ha1.term == ha2.term);
  assert(ha2.term == ha3.term);
  assert(ha3.term == ha4.term);

  Format.printf "Peano OK@."

let lambda_tests () =
  (* The lambda calculus uses binders, it can be more tricky.
     There is no constant, but variables (free or not).
     We have four operators :
     - Var : Variable -> Term
     - App : Term * Term -> Term
     - Lambda : [Variable]. Term -> Term
     - Subst : [Variable]. Term * Term -> Term
  *)


  Format.printf "Starting Lambda-calculus hashconsing tests@.";

  (* First, we try on two identical id functions, modulo apha-conversion *)
  let bx = DBinder (None, "x") in
  let id1 = DTerm (None, "Lambda", [bx; DVar (None, "x")]) in
  let by = DBinder (None, "y") in
  let id2 = DTerm (None, "Lambda", [by; DVar (None, "y")]) in

  let hid1 = create_term id1 in
  let hid2 = create_term id2 in

  assert (hid1.term == hid2.term);

  (* Term for getting the second element of a pair in lambda calculus, which is
  also the encoding for "false". *)
  let lsnd = DTerm (None, "Lambda",
                   [bx; DTerm (None, "Lambda", [by; DVar (None, "y")])]) in
  let hsnd = create_term lsnd in

  (* We open the term and get the second lambda, which is basically id *)
  let hid3 = match hsnd.term with
    | HTerm (_, hl) ->
      (List.hd @@ List.tl hl).value
    | _ -> assert false in

  assert (hid2.term == hid3);

  Format.printf "Lambda calculs OK@."

let reconstruct_tests () =
  (* Tests the reconstruction of the hterms into terms
     *)

  Format.printf "Starting reconstruction tests@.";

  let bx = DBinder (None, "x") in
  let by = DBinder (None, "y") in
  let bz = DBinder (None, "z") in
  let id3 = DTerm (None, "Lambda", [bz; DVar (None, "z")]) in

  let lsnd = DTerm (None, "Lambda",
                    [bx; DTerm (None, "Lambda", [by; DVar (None, "y")])]) in
  let hsnd = create_term lsnd in
  let snd = create_dterm hsnd in

  assert (lsnd = snd);

  let app = DTerm (None, "App", [lsnd; id3]) in
  let happ = create_term app in
  let recr_app = create_dterm happ in

  assert (app = recr_app);

  Format.printf "Term reconstruction OK@."


let _ =
  create_tests ();
  peano_tests ();
  lambda_tests ();
  reconstruct_tests ()
