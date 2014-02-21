(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierrick Couderc
*)

open Term_ast_dag
open Term_system_error

type id = int
type ident = string
(* type hash = int *)

type 'a hashed = { id : int; hash : int; value : 'a}

type 'a hlist = 'a hashed list

let nil = []

let nil2 = []

type hterm_raw =
  | HConst of ident
  | HTerm of ident * hterm_raw hlist
  | HBinder of (id * id) list (* the first parameter is only for hashconsing,
                                 while the second one refers to the id of the
                                 head of an hashconsed list. This will change
                                 for a 'id hashed list', but there are problems
                                 to solve first because that breaks the hashconsing *)
  | HVar
  | HFreeVar of ident

type hterm = { term: hterm_raw; binders: string list }

let rec hash_hlist = function
    | [] -> 0
    | v :: [] -> hash_term v.value
    | v1 :: v2 :: _ -> (hash_term v1.value) + 17 * v2.hash

(** The hashtbl's hash function is efficient on string, so we can use it without
  any consequence *)
and hash_term = function
  | HConst i -> Hashtbl.hash i
  | HFreeVar i -> 2 + Hashtbl.hash i
  | HTerm (i, htl) -> (3 + Hashtbl.hash i) + hash_hlist htl
  | HVar -> 4 (* a var is unique and defined by its binder *)
  | HBinder b -> 5 + Hashtbl.hash b (* Hashing a int list is efficient *)


let rec equal_hlist hl1 hl2 =
  match hl1, hl2 with
  | [], [] -> true
    (* We can suppose at the moment we try the equality that tl1 and tl2 are
      already hashconsed *)
  | v1 :: tl1, v2 :: tl2 -> v1.value == v2.value && tl1 == tl2
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
    type t = hterm_raw
    let equal = equal
    let hash = hash_term
  end)


module HListtbl = Hashtbl.Make(struct
    type t = hterm_raw hlist
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
  | DBinder id -> (id, ref []) :: bindings
  | _ -> bindings

let remove_binder t bindings =
  match t with
  | DBinder id -> List.remove_assoc id bindings
  | _ -> bindings

let string_of_bindings b =
  List.fold_left (fun acc (i, _) -> Format.sprintf "%s, %s" acc i) "" b

let is_var t =
  match t with
  | DVar _ -> true
  | _ -> false

let get_var_id t =
  match t with
  | DVar id -> id
  | _ -> assert false

let add_binder_name t l =
  match t with
  | DBinder id -> id :: l
  | _ -> l

(* Not tailrec, but an operator doesn't have thousands of subterm. It evaluates
   from right to left actually, to retrieve the binded variables before
   hashconsing the binder. *)
let rec create_hlist bindings names = function
  | [] -> nil, names
  | hd :: tl ->
    (* If it's a binder, we add it in our bindings *)
    let bindings = add_binder hd bindings in

    (* We evaluate the rest of the list *)
    let htl, names = create_hlist bindings names tl in
    let names = add_binder_name hd names in

    (* we can evaluate the actual term *)
    let term, bindings, names = create_term_raw bindings names hd in

    (* Format.printf "[%s]@." @@ String.concat ", " names; *)

    (* we create a new list with our term *)
    let res = create_cons term htl in
    if is_var hd && List.mem_assoc (get_var_id hd) bindings then
      begin
        let id = (List.hd res).id in
        let r = try List.assoc (get_var_id hd) bindings
          with Not_found ->
            raise (TermSystemError (VariableUnbound, get_var_id hd)) in
        r := id :: !r;
        res, names
      end
    else res, names


and create_cons =
  let open HListtbl in
  let id = ref 1 in
  let t = create 43 in
  add t nil nil;
  fun term hlist ->
    let new_hl =
      { id = !id; value = term;
        hash = hash_term term + 17 * hash_hlist hlist} :: hlist in
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
  fun (bindings : bindings) names td ->
    let term, bindings, names =
      match td with
      | DConst i -> HConst i, bindings, names
      | DVar id when not (List.mem_assoc id bindings) -> HFreeVar id, bindings, names
      | DVar id -> hvar, bindings, names
      | DTerm (id, l) ->
        let hl, names = create_hlist bindings names l in
        HTerm (id, hl), bindings, names
      | DBinder id ->
        let binded = !(List.assoc id bindings) in
        let binded = create_id_list binded in
        HBinder binded, remove_binder td bindings, names
    in

    (* Should we add the current bindings in the hashtbl ? Technically no, we
       need some case were it is mandatory in case this is needed *)
    try find term_tbl term, bindings, names
    with Not_found ->
      add term_tbl term term;
      term, bindings, names

and create_id_list_raw =
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

and create_id_list l =
  match l with
  | [] -> nil2
  | id :: tl -> create_id_list_raw id (create_id_list tl)


(* Main function to hashcons a term_dag *)
(* let create_term td = *)
(*   let term, _, _ = create_term_raw [] [] td in *)
(*   term *)

let create_term td =
  let term, _, binders = create_term_raw [] [] td in
  { term; binders }

module IMap = Map.Make (struct
    type t = int
    let compare = Pervasives.compare
  end)

let create_dterm td =
  let td, names = td.term, td.binders in
  let rec step names bindings cell td =
    match td, names with
    | HConst i, _ -> DConst i, names, bindings
    | HVar, _ -> DVar (IMap.find cell bindings), names, bindings
    | HFreeVar i, _ -> DVar i, names, bindings
    | HBinder binded, var :: names ->
      DBinder var, names, List.fold_left
        (fun b (_, id) -> IMap.add id var bindings)
        bindings binded
    | HTerm (i, terms), names ->
      let terms, names, bindings = List.fold_left
          (fun (l, names, bindings) t ->
             let term, names, bindings = step names bindings t.id t.value in
             term :: l, names, bindings) ([], names, bindings) terms in
      DTerm (i, List.rev terms), names, bindings
    | _, _ -> assert false
  in
  let t, _, _ = step names IMap.empty (-1) td in t

(* Pretty printing function *)

let rec string_of_hlist hl names =
  let rec step acc = function
  | [] -> ""
  | [ht] -> Format.sprintf "%s%s" acc (string_of_hterm names ht.value)
  | ht :: tl -> step (Format.sprintf "%s%s, " acc (string_of_hterm names ht.value)) tl
  in
  step "" hl

and string_of_idlist il =
  let rec step acc = function
  | [] -> ""
  | [(_, i)] -> Format.sprintf "%s%d" acc i
  | (_, i) :: tl -> step (Format.sprintf "%s%d, " acc i) tl
  in
  step "" il

and string_of_hterm names = function
  | HConst i -> i
  | HVar -> "#"
  | HFreeVar i -> i
  | HBinder binded -> Format.sprintf "[.\\{%s}]" @@ string_of_idlist binded
  | HTerm (i, hl) -> Format.sprintf "%s(%s)" i (string_of_hlist hl names)

let pretty_print_list hl =
  Format.printf "[%s]@." @@ string_of_hlist hl []

let pretty_print hterm =
  print_endline @@ string_of_hterm [] hterm

let pretty_print_with_names names term = assert false

(* Dot representation, which is well suited to show sharing *)

module ReprH = Hashtbl.Make (struct
    type t = hterm_raw
    let hash = hash_term
    let equal = (==)
  end)

let dot t filename =
  let term_tbl = Hashtbl.create 19 in
  let repr_tbl = ReprH.create 19 in
  let is_binder = function HBinder _ -> true | _ -> false in
  let is_var = function HVar -> true | _ -> false in
  let binder_id = ref 0 in
  let term_id = ref 0 in
  let repr t =
    try
      ReprH.find repr_tbl t
    with Not_found ->
      let r = match t with
        | HVar -> "var"
        | HFreeVar ident -> ident
        | HBinder _ ->
          let res = Format.sprintf "[binder:%d]" !binder_id in
          incr binder_id;
          res
        | HConst ident -> ident
        | HTerm (n, _) ->
          let res = Format.sprintf "%s:%d" n !term_id in
          incr term_id;
          res in
      ReprH.add repr_tbl t r;
      r
  in
  let rec browse id t =
    let key = repr t in
    if not (Hashtbl.mem term_tbl key) || is_binder t then
      match t with
      | HTerm (n, args) -> let _, value = List.fold_left (fun (n, acc) t ->
          let r = if is_var t.value then Format.sprintf "%s:%d" (repr t.value) t.id
            else repr t.value in
          n + 1, Format.sprintf "%s\"%s\" -> \"%s\"[label=arg%d];@\n" acc key r n)
          (1, "") args in
        Hashtbl.add term_tbl key value;
        List.iter (fun t -> browse t.id t.value) args
      | HBinder binded -> let value = List.fold_left (fun acc (_, id) ->
          Format.sprintf "%s\"%s\" -> \"var:%d\"[style=dotted]@\n" acc key id)
          "" binded in
        Hashtbl.add term_tbl key value
      | _ -> Hashtbl.add term_tbl key ""
  in
  browse (-1) t;
  let f = open_out filename in
  output_string f "digraph output {\n";
  Hashtbl.iter (fun _ value -> output_string f value) term_tbl;
  output_string f "}\n";
  close_out f
