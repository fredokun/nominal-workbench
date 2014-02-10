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
  | HBinder of id
  | HVar of id
  | HFreeVar of ident

let rec hash_hlist l =
  let rec step acc = function
    | [] -> acc
    | (id, hd) :: tl -> step ((hash hd) + 17 * acc) tl in
  step 1 l

(** The hashtbl's hash function is efficient on string, so we can use it without
  any consequence *)
and hash = function
  | HConst i -> Hashtbl.hash i
  | HFreeVar i -> 2 + Hashtbl.hash i
  | HTerm (i, htl) -> (3 + Hashtbl.hash i) + hash_hlist htl
  | HVar i -> 4 + Hashtbl.hash i
  | HBinder b -> 5 + Hashtbl.hash b


let rec equal_hlist hl1 hl2 =
  match hl1, hl2 with
  | [], [] -> true
    (* We can suppose at the moment we try the equality that tl1 and tl2 are
      already hashconsed *)
  | (_, hd1) :: tl1, (_, hd2) :: tl2 -> hd1 == hd2 && tl1 == tl2
  | _, _ -> false

and equal ht1 ht2 =
  match ht1, ht2 with
  | HConst i1, HConst i2 | HFreeVar i1, HFreeVar i2 -> i1 = i2
  | HTerm (i1, htl1), HTerm (i2, htl2) ->
    i1 = i2 && equal_hlist htl1 htl2
  | HVar v1, HVar v2 -> v1 == v2
  (* Two binders are equal if their ref is the same *)
  | HBinder b1, HBinder b2 -> b1 == b2
  | _, _ -> false


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
let rec create_hlist bind bindings = function
  | [] -> nil
  | hd :: tl ->
    let term, bind, bindings = create_term_raw bind bindings hd in
    create_cons term (create_hlist bind bindings tl)

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

and create_term_raw : 'a -> 'b -> 'c -> 'e * 'f * 'g =
  (* let binder = HBinder (ref []) in *)
  let open HTermtbl in
  let term_tbl = create 43 in
  fun bind bindings td ->
    let term, bind, bindings =
      match td with
      | DConst i -> HConst i, bind, bindings
      | DVar (id, Some _) -> HVar (SMap.find id bindings), bind, bindings
      (* Some computations to do for de Bruijn *)
      | DVar (id, None) -> HFreeVar id, bind, bindings
      | DTerm (id, l) ->
        let hl = create_hlist bind bindings l in
        HTerm (id, hl), bind, bindings

      (* The hashconsing of the binder is a problem for now, since we need its binded
         variable to instantiate is *)
      | DBinder (id, _) ->
        let bindings = SMap.add id bind bindings in
        HBinder bind, bind+1, bindings in

    (* Should we add the current bindings in the hashtbl ? Technically no, we
       need some case were it is mandatory *)
    try find term_tbl term, bind, bindings
    with Not_found ->
      add term_tbl term term;
      term, bind, bindings

let create_term td =
  let term, _, _ = create_term_raw 0 SMap.empty td in
  term

let rec string_of_hlist hl =
  let rec step acc = function
  | [] -> ""
  | [(_, ht)] -> Format.sprintf "%s %s" acc (string_of_hterm ht)
  | (_, ht) :: tl -> step (Format.sprintf "%s%s, " acc (string_of_hterm ht)) tl
  in
  step "" hl

and string_of_hterm = function
  | HConst i -> i
  | HVar i -> string_of_int i
  | HFreeVar i -> i
  | HBinder i -> Format.sprintf "[%d]" i
  | HTerm (i, hl) -> Format.sprintf "%s(%s)" i (string_of_hlist hl)


let pretty_print hterm =
  print_endline @@ string_of_hterm hterm
