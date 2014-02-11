(** Highly experimental version of the hashconsed Term_ast /!\
    For now, it's only for testing and design purpose, please
*)

open Term_ast_dag
open Term_system_error

type id = int
type ident = string

type 'a hlist = (id * 'a) list

let nil = []

let nil2 = []

type hterm =
  | HConst of ident
  | HTerm of ident * hterm hlist
  | HBinder of (id * id) list
  | HVar
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
  | HVar -> 4 (* a var is unique and defined by its binder *)
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
  | HVar, HVar -> true
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

module IdListtbl = Hashtbl.Make(
    struct
      type t = (id * id) list

      let equal il1 il2 =
        match il1, il2 with
        | [], [] -> true
        | (_, hd1) :: tl1, (_, hd2) :: tl2 -> hd1 = hd2 && tl1 == tl2
        | _, _ -> false

      let hash l =
        let rec step acc = function
          | [] -> acc
          | (_, id) :: tl -> step (id + 17 * acc) tl in
        step 1 l
    end)

module SMap = Map.Make(String)

(* We have to use a assoc list since a Map removes its previous binding for a
   already defined id. *)
type bindings = (ident * id list ref) list

let add_binder t bindings =
  match t with
  | DBinder (id, _) -> (id, ref []) :: bindings
  | _ -> bindings

let remove_binder t bindings =
  match t with
  | DBinder (id, _) -> List.remove_assoc id bindings
  | _ -> bindings

let string_of_bindings b =
  List.fold_left (fun acc (i, _) -> Format.sprintf "%s, %s" acc i) "" b

let is_var = function
  | DVar (_, Some _) -> true
  | _ -> false

let get_var_id = function
  | DVar (id, Some _) -> id
  | _ -> assert false

(* Not tailrec, but an operator doesn't have thousands of subterm. It evaluates
   from right to left actually, to retrieve the binded variables. *)
let rec create_hlist bindings = function
  | [] -> nil
  | hd :: tl ->
    (* If it's a binder, we add it in our bindings *)
    let bindings = add_binder hd bindings in

    (* We evaluate the rest of the list *)
    let htl = create_hlist bindings tl in

    (* we can evaluate the actual term *)
    let term, (bindings : bindings) = create_term_raw bindings hd in

    (* we create a new list with our term *)
    let res = create_cons term htl in
    if is_var hd then
      begin
        let (id, _) = List.hd res in
        let r = List.assoc (get_var_id hd) bindings in
        r := id :: !r;
        res
      end
    else res


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

and create_term_raw =
  let open HTermtbl in
  let term_tbl = create 43 in
  let hvar = HVar in
  add term_tbl hvar hvar;
  fun (bindings : bindings) td ->
    let term, bindings =
      match td with
      | DConst i -> HConst i, bindings
      | DVar (id, Some _) -> hvar, bindings
      | DVar (id, None) -> HFreeVar id, bindings
      | DTerm (id, l) ->
        let hl = create_hlist bindings l in
        HTerm (id, hl), bindings
      | DBinder (id, _) ->
        let binded : id list = !(List.assoc id bindings) in
        let binded : (id * id) list = create_id_list binded in
        HBinder binded, remove_binder td bindings
    in

    (* Should we add the current bindings in the hashtbl ? Technically no, we
       need some case were it is mandatory in case this is needed *)
    try find term_tbl term, bindings
    with Not_found ->
      add term_tbl term term;
      term, bindings

and create_id_list_raw : id -> (id * id) list -> (id * id) list =
  let open IdListtbl in
  let t = create 19 in
  let lid = ref 0 in
  add t nil2 nil2;
  fun id hl ->
    let hl = (!lid, id) :: hl in
    try
      find t hl
    with Not_found ->
      add t hl hl;
      incr lid;
      hl

and create_id_list (l : id list) : (id * id) list =
  match l with
  | [] -> nil2
  | id :: tl -> create_id_list_raw id (create_id_list tl)


(* Main function to hashcons a term_dag *)
let create_term td =
  let term, _ = create_term_raw [] td in
  term


(* Pretty printing function *)

let rec string_of_hlist hl =
  let rec step acc = function
  | [] -> ""
  | [(_, ht)] -> Format.sprintf "%s%s" acc (string_of_hterm ht)
  | (_, ht) :: tl -> step (Format.sprintf "%s%s, " acc (string_of_hterm ht)) tl
  in
  step "" hl

and string_of_idlist il =
  let rec step acc = function
  | [] -> ""
  | [(_, i)] -> Format.sprintf "%s%d" acc i
  | (_, i) :: tl -> step (Format.sprintf "%s%d, " acc i) tl
  in
  step "" il

and string_of_hterm = function
  | HConst i -> i
  | HVar -> "#"
  | HFreeVar i -> i
  | HBinder binded -> Format.sprintf "[.\\{%s}]" @@ string_of_idlist binded
  | HTerm (i, hl) -> Format.sprintf "%s(%s)" i (string_of_hlist hl)

let pretty_print_list hl =
  Format.printf "[%s]@." @@ string_of_hlist hl

let pretty_print hterm =
  print_endline @@ string_of_hterm hterm
