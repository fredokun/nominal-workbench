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

module SMap = Map.Make(String)

(* Not tailrec, but an operator doesn't have thousands of subterm *)
let rec create_hlist bindings = function
  | [] -> nil
  | hd :: tl ->
    let term, bindings = create_term bindings hd in
    create_cons term (create_hlist bindings tl)

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
  let binder = HBinder (ref []) in
  let open HTermtbl in
  let term_tbl = create 43 in
  fun bindings td ->
    let term, bindings =
      match td with
      | DConst i -> HConst i, bindings
      | DVar (id, Some _) -> HVar (SMap.find id bindings), bindings
      (* Some computations to do for de Bruijn *)
      | DVar (id, None) -> HFreeVar id, bindings
      | DTerm (id, l) ->
        let hl = create_hlist bindings l in
        HTerm (id, hl), bindings

      (* The hashconsing of the binder is a problem for now, since we need its binded
         variable to instantiate is *)
      | DBinder (id, _) ->
        let bindings = SMap.add id 0 @@
          SMap.map (fun i -> i + 1) bindings in
        binder, bindings in

    (* Should we add the current bindings in the hashtbl ? Technically no, we
       need some case were it is mandatory *)
    try find term_tbl term, bindings
    with Not_found ->
      add term_tbl term term;
      term, bindings
