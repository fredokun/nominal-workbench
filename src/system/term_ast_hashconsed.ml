(* Distributed under the MIT License.
  (See accompanying file LICENSE.txt)
  (C) Copyright Pierrick Couderc
*)

open Term_ast_typed
open Term_system_error

type id = int
type ident = string

type 'a hashed = { id : int; hash : int; value : 'a}

type 'a hlist = 'a hashed list

let nil = []

let nil2 = []

type hterm_raw =
  | HConst of ident
  | HTerm of ident * hterm_raw hlist
  | HBinder of id hlist (* refers to the id of the head of an hashconsed
                           list. *)
  | HVar of int
  | HFreeVar of ident

type hterm_name =
  | NTerm of hterm_name list
  | NName of string
  | NAny

type hterm = { term: hterm_raw; binders: hterm_name }

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
  | HVar i -> i (* a var is unique and defined by its binder *)
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
  | HVar i1, HVar i2 -> i1 = i2
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

let hash_idlist = function
  | [] -> 0
  | id :: [] -> id.value
  | id1 :: id2 :: _ -> id1.value + 17 * id2.hash

module IdListtbl = Hashtbl.Make(
    struct
      type t = id hlist

      let equal il1 il2 =
        match il1, il2 with
        | [], [] -> true
        | hd1 :: tl1, hd2 :: tl2 -> hd1.value = hd2.value && tl1 == tl2
        | _, _ -> false

      let hash = hash_idlist
    end)

module SMap = Map.Make(String)

(* We have to use a assoc list since a Map removes its previous binding for a
   already defined id. *)
type bindings = (ident * id list ref) list

let add_binder t bindings =
  match t with
  | DBinder (_, id) -> (id, ref []) :: bindings
  | _ -> bindings

let add_index t index =
  match t with
  | DBinder (_, id) -> (id, 0) :: (List.map (fun (id, v) -> (id, v+1)) index)
  | _ -> index

let remove_binder t bindings =
  match t with
  | DBinder (_, id) -> List.remove_assoc id bindings
  | _ -> bindings

let string_of_bindings b =
  List.fold_left (fun acc (i, _) -> Format.sprintf "%s, %s" acc i) "" b

let is_var t =
  match t with
  | DVar _ -> true
  | _ -> false

let get_var_id t =
  match t with
  | DVar (_, id) -> id
  | _ -> assert false

let is_binder = function
  | DBinder _ -> true
  | _ -> false

let get_binder_id = function
  | DBinder (_, id) -> id
  | _ -> assert false

let add_binder_name t l =
  match t with
  | DBinder (_, id) -> NName id :: l
  | _ -> l

(* Not tailrec, but an operator doesn't have thousands of subterm. It evaluates
   from right to left actually, to retrieve the binded variables before
   hashconsing the binder. *)
let rec create_hlist bindings names index = function
  | [] -> nil, []
  | hd :: tl ->
    (* If it's a binder, we add it in our bindings *)
    let bindings = add_binder hd bindings in
    let index = add_index hd index in

    (* We evaluate the rest of the list *)
    let htl, names = create_hlist bindings names index tl in

    (* we can evaluate the actual term *)
    let term, bindings, name = create_term_raw bindings names index hd in
    let names = name :: names in

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
  (* let hvar = HVar in *)
  (* add term_tbl hvar hvar; *)
  fun (bindings : bindings) names index td ->
    let term, bindings, names =
      match td with
      | DConst (_, i) -> HConst i, bindings, NAny
      | DVar (_, id) when not (List.mem_assoc id bindings) -> HFreeVar id, bindings, NAny
      | DVar (_, id) -> HVar (List.assoc id index), bindings, NName id
      | DTerm (_, id, l) ->
        let hl, names = create_hlist bindings names index l in
        HTerm (id, hl), bindings, NTerm names
      | DBinder (_, id) ->
      (* We go through the operators sub-terms from right to left so we already have added the binded variables.
         in the bindings list. *)
        let binded = !(List.assoc id bindings) in
        let binded = create_id_list binded in
        HBinder binded, remove_binder td bindings, NName id
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
    let new_hl = { id = !lid; value = id;
                  hash = id + 17 * hash_idlist hl } :: hl in
    try
      find t new_hl
    with Not_found ->
      add t new_hl new_hl;
      incr lid;
      new_hl

and create_id_list l =
  match l with
  | [] -> nil2
  | id :: tl -> create_id_list_raw id (create_id_list tl)


let create_term td =
  let term, _, binders = create_term_raw [] [] [] td in
  { term; binders }

module IMap = Map.Make (struct
    type t = int
    let compare = Pervasives.compare
  end)

let create_dterm td =
  let td, names = td.term, td.binders in
  let rec step names td =
    match td, names with
    | HConst i, _ -> DConst (None, i)
    | HVar _, NName n -> DVar (None, n)
    | HFreeVar i, _ -> DVar (None, i)
    | HBinder binded, NName n -> DBinder (None, n)
    | HTerm (i, terms), NTerm names ->
      let terms = List.map2 (fun t n -> step n t.value) terms names in
      DTerm (None, i, terms)
    | _, _ -> assert false
  in
  step names td

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
  | [i] -> Format.sprintf "%s%d" acc i.value
  | i :: tl -> step (Format.sprintf "%s%d, " acc i.value) tl
  in
  step "" il

and string_of_hterm names = function
  | HConst i -> i
  | HVar i -> "#" ^ string_of_int i
  | HFreeVar i -> i
  | HBinder binded -> Format.sprintf "[.\\{%s}]" @@ string_of_idlist binded
  | HTerm (i, hl) -> Format.sprintf "%s(%s)" i (string_of_hlist hl names)

let rec string_of_hterm_name = function
  | NAny -> "_"
  | NName n -> n
  | NTerm n -> Format.sprintf "(%s)" @@
    String.concat "," @@ List.map string_of_hterm_name n

let pretty_print_list hl =
  Format.printf "[%s]@." @@ string_of_hlist hl []

let pretty_print hterm =
  print_endline @@ string_of_hterm [] hterm

let pretty_print_with_names names term = assert false

let pretty_print_names name =
  print_endline @@ string_of_hterm_name name

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
  let is_var = function HVar _ -> true | _ -> false in
  let binder_id = ref 0 in
  let term_id = ref 0 in
  let repr t =
    try
      ReprH.find repr_tbl t
    with Not_found ->
      let r = match t with
        | HVar _ -> "var"
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
      | HBinder binded -> let value = List.fold_left (fun acc id ->
          Format.sprintf "%s\"%s\" -> \"var:%d\"[style=dotted]@\n" acc key id.value)
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
