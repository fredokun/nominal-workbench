(** Highly experimental version of the hashconsed Term_ast /!\
    For now, it's only for testing and design purpose, please
*)

open Term_ast_dag

type id = int
type ident = string

type 'a hlist =
  | HNil
  | HCons of id * 'a * 'a hlist

let nil = HNil

type hterm =
  | HConst of ident
  | HTerm of ident * hterm hlist
  | HBinder of hterm list ref
  | HVar of int
  | HFreeVar of ident
  (* | HNone (\* used by the binder *\) *)

(* (hterm -> int) -> hterm hlist -> int *)
let rec hash_hlist f =
  let rec step acc = function
    | HNil -> acc
    | HCons (id, hd, tl) -> step (id + (f hd) + 17 * acc) tl in
  step 1

(** The hashtbl's hash function is efficient on string, so we can use it without
  any consequence *)
and hash = function
  | HConst i -> Hashtbl.hash i
  | HFreeVar i -> 2 + Hashtbl.hash i
  | HTerm (i, htl) -> (3 + Hashtbl.hash i) + hash_hlist hash htl
  | HVar _ | HBinder _ -> assert false (* to determine *)


let rec equal_hlist hl1 hl2 =
  match hl1, hl2 with
  | HNil, HNil -> true
    (* We can suppose at the moment we try the equality that tl1 and 2 are
      already hashconsed *)
  | HCons (_, hd1, tl1), HCons (_, hd2, tl2) -> equal hd1 hd2 && tl1 == tl2
  | _, _ -> false

and equal ht1 ht2 =
  match ht1, ht2 with
  | HConst i1, HConst i2 | HFreeVar i1, HFreeVar i2 -> i1 = i2
  | HTerm (i1, htl1), HTerm (i2, htl2) ->
    i1 = i2 && equal_hlist htl1 htl2
  | _, _ -> assert false


module HTermtbl = Hashtbl.Make(struct
    type t = hterm
    let equal = equal
    let hash = hash
  end)

let term_tbl = HTermtbl.create 43

module HListtbl = Hashtbl.Make(struct
    type t = hterm hlist
    let equal = equal_hlist
    let hash = hash_hlist hash
  end)

(* Not tailrec, but an operator doesn't have thousands of subterm *)
let rec create_hlist = function
  | [] -> nil
  | hd :: tl -> create_cons (create_term hd) (create_hlist tl)

and create_cons =
  let open HListtbl in
  let id = ref 1 in
  let t = create 43 in
  add t nil nil;
  fun term hlist ->
    let new_hl = HCons (!id, term, hlist) in
    try find t new_hl
    with Not_found ->
      add t new_hl new_hl;
      incr id;
      new_hl

(** All those functions will be of course refactored into one *)

and create_const cst =
  let open HTermtbl  in
  (* let t = .create 43 in *)
  let cst = HConst cst in
  try find term_tbl cst
  with Not_found ->
    add term_tbl cst cst;
    cst

and create_binder id =
  let open HTermtbl in
  let binded = ref [] in
  let bin = HBinder binded in
  try find term_tbl bin
  with Not_found ->
    add term_tbl bin bin;
    bin

and create_var i =
  let open HTermtbl in
  let v = HVar i in
  try find term_tbl v
  with Not_found ->
    add term_tbl v v;
    v

and create_freevar id =
  let open HTermtbl in
  let fv = HFreeVar id in
  try find term_tbl fv
  with Not_found ->
    add term_tbl fv fv;
    fv

and create_hterm id l =
  let open HTermtbl in
  let hl = create_hlist l in
  let term = HTerm (id, hl) in
  try find term_tbl term
  with Not_found ->
    add term_tbl term term;
    term

and create_term td =
  match td with
  | DConst i -> create_const i
  | DVar (id, Some _) -> create_var 42 (* Some computations to do for de Bruijn *)
  | DVar (id, None) -> create_freevar id
  | DTerm (id, l) -> create_hterm id l
  | DBinder (id, _) -> create_binder id
  (* each constructor has its own hashconsing function to get its equivalent *)
