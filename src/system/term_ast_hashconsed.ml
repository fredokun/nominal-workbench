(** Highly experimental version of the hashconsed Term_ast /!\
    For now, it's only for testing and design purpose, please
*)

open Term_ast_dag

type id = int
type ident = string

type 'a hlist = (id * 'a) list

let nil = []

type hterm =
  | HConst of ident
  | HTerm of ident * hterm hlist
  | HBinder of hterm list ref
  | HVar of int
  | HFreeVar of ident

let rec hash_hlist l =
  let rec step acc = function
    | [] -> acc
    | (id, hd) :: tl -> step (id + (hash hd) + 17 * acc) tl in
  step 1 l

(** The hashtbl's hash function is efficient on string, so we can use it without
  any consequence *)
and hash = function
  | HConst i -> Hashtbl.hash i
  | HFreeVar i -> 2 + Hashtbl.hash i
  | HTerm (i, htl) -> (3 + Hashtbl.hash i) + hash_hlist htl
  | HVar _ | HBinder _ -> assert false (* to determine *)


let rec equal_hlist hl1 hl2 =
  match hl1, hl2 with
  | [], [] -> true
    (* We can suppose at the moment we try the equality that tl1 and 2 are
      already hashconsed *)
  | (_, hd1) :: tl1, (_, hd2) :: tl2 -> equal hd1 hd2 && tl1 == tl2
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


module HListtbl = Hashtbl.Make(struct
    type t = hterm hlist
    let equal = equal_hlist
    let hash = hash_hlist
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
    let new_hl = (!id, term) :: hlist in
    try find t new_hl
    with Not_found ->
      add t new_hl new_hl;
      incr id;
      new_hl

and create_term =
  let binder = HBinder (ref [])
  let open HTermtbl in
  let term_tbl = create 43 in
  fun td ->
    let term =
      match td with
      | DConst i -> HConst i
      | DVar (id, Some _) -> HVar 42 (* Some computations to do for de Bruijn *)
      | DVar (id, None) -> HFreeVar id
      | DTerm (id, l) ->
        let hl = create_hlist l in
        HTerm (id, hl)

(* The hashconsing of the binder is a problem for now, since we need its binded
  variable to instantiate is *)
      | DBinder (id, _) ->  binder(* HBinder (ref []) *) in
    try find term_tbl term
    with Not_found ->
      add term_tbl term term;
      term
(* each constructor has its own hashconsing function to get its equivalent *)
