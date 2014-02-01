open Spotlib.Spot
open List.Infix

open Asttypes
open Longident
open Parsetree
open Xtype
module I = Ident

open Option.Open
let fail = None

module PathLimit = struct

  let decr ?(by=1) score =
    let score = score - by in
    if score < 0 then None
    else Some score

end

module TypeLimit = struct
  type t = { score : int; expansions: int }

  let create score = { score; expansions = 10 }

  let decr ?(by=1) ({ score } as desc) =
    let score = score - by in
    if score < 0 then None
    else Some { desc with score }

  let max t1 t2 =
    match t1, t2 with
    | None, None -> None
    | None, _ -> t2
    | _, None -> t1
    | Some { score= s1 }, Some { score= s2 } ->
        (* we ignore expansion *)
        if s1 >= s2 then t1 else t2

  let maxs = List.fold_left1 max

end

let match_name n m limit = 
  match n with
  | "_" | "_*_" -> return limit
  | "(_)" when 
      begin try match m.[0] with 
      | 'A'..'Z' | 'a'..'z' | '_' | '0'..'9' | '#' (* class type *) -> false
      | _ -> true
        with _ -> false end ->
      return limit
  | _ -> 
      if n = m then return limit
      else if String.lowercase n = String.lowercase m then return limit
      (* CR jfuruse: we can have better package name match. *)
      else if m.[0] = '{' && String.lowercase (String.sub m 1 (String.length m - 2)) = String.lowercase n then return limit
      else fail

let match_package n phack limit =
  let ps = Packageshack.to_packages phack in
  List.fold_left (fun st p ->
    match st, match_name n (OCamlFind.Package.name p) limit with
    | None, x -> x
    | (Some _ as o), None -> o
    | Some v, Some v' -> Some (max v v')
  ) None ps

(* CRv2 We should use edit distance, cached. *)
let rec match_path li p limit = 
  let open Ppath in
  let open PathLimit in
  match li, p with
  | Ldot(n1, "_*_"), PPdot(m1, _m2) ->
      max (match_path n1 m1 limit) (match_path li m1 limit)
  | Lident n,   PPpack phack ->
      match_package n phack limit
  | Lident n,   PPdot(_, m)
  | Lident n,   PPident m ->
      match_name n m limit
  | Ldot(n1, n2), PPdot(m1, m2) ->
      match_name n2 m2 limit >>= match_path n1 m1
  | Lapply (li1, _li2), PPapply (p1, _p2) ->
      (* A(B) matches with A(C) *)
      match_path li1 p1 limit 
  | li, PPapply (p1, _p2) ->
      (* A matches with A(C) but with slight penalty *)
      decr limit >>= match_path li p1
  | _ -> fail

let dummy_core_type_var = { ptyp_desc= Ptyp_any;
                            ptyp_loc= Location.none }
let dummy_type_expr_var = Var (-1)

let rec get_core_arrows acc pty = match pty.ptyp_desc with
  | Ptyp_arrow (l, t, t') -> get_core_arrows ((l,t)::acc) t'
  | _ -> List.rev acc, pty

let rec get_target_arrows acc ty =match ty with
  | Arrow (l, t, t') -> get_target_arrows ((l,t)::acc) t'
  | _ -> List.rev acc, ty

let rec size_core_type pty = match pty.ptyp_desc with
  | Ptyp_any -> 0
  | Ptyp_var _ -> 0
  | Ptyp_arrow (_, t1, t2) ->
      size_core_type t1
      + size_core_type t2
      + 1
  | Ptyp_tuple tys -> List.sum & List.map size_core_type tys
  | Ptyp_constr (_, tys) -> 1 + List.sum (List.map size_core_type tys)
  | Ptyp_object cfts -> List.sum & List.map size_core_field_type cfts
  | Ptyp_class (_, ctys, _) -> 1 + List.sum (List.map size_core_type ctys)
  | Ptyp_alias (cty, _) -> size_core_type cty
  | Ptyp_variant (rfs, _, _) -> List.sum & List.map size_row_field rfs
  | Ptyp_poly (vars, cty) -> List.length vars + size_core_type cty
  | Ptyp_package pty -> size_package_type pty

and size_core_field_type cft = match cft.pfield_desc with
  | Pfield (_, cty) -> size_core_type cty
  | Pfield_var -> 0

and size_row_field = function
  | Rtag (_, _, ctys) -> 1 + List.sum (List.map size_core_type ctys)
  | Rinherit cty -> size_core_type cty + 1

and size_package_type (_, lctys) =
  1 + List.sum (List.map (size_core_type ** snd) lctys)

(* I never see good result with no_target_type_instantiate=false *)
let match_type ?(no_target_type_instantiate=true) pattern target limit =
  let open TypeLimit in
  let rec match_type pattern target limit =
    (* pattern = tvar is treated specially since it always returns the maximum score w/o changing the pattern *)
    match pattern.ptyp_desc, target with
    | (Ptyp_any | Ptyp_var _), (Var _ | VarNamed _ | Univar _ | UnivarNamed _) -> 
        return limit
    | Ptyp_any, _ -> decr limit (* Any matches anything but with a slight penalty. No real unification. *)
  
    | _, Link { contents = `Linked ty } -> 
        decr limit >>= match_type pattern ty
    | _, Link _ -> fail
    | _ ->
  
    maxs [
      remove_target_type_option pattern target limit;
      make_tuple_left  pattern target limit;
      make_tuple_right pattern target limit;
      match_arrow_types pattern target limit;

      match_alias pattern target limit;

      (match pattern.ptyp_desc, target with
  
      | Ptyp_tuple ts1, Tuple ts2 -> match_types ts1 ts2 limit
  
      | Ptyp_constr (li, ts1), Constr ((p2, _), ts2) -> (* CR jfuruse: TODO *)
          match_path li.txt p2 limit.score
          >>= fun score -> match_types ts1 ts2 { limit with score }

      | _, (Var _ | VarNamed _ | Univar _ | UnivarNamed _) ->
          decr ~by:(if no_target_type_instantiate then 1000 
                    else size_core_type pattern) limit
  
      | _ -> fail)
    ]
  
  and match_alias pattern target limit =
    if limit.expansions <= 0 then fail
    else
      match target with
      | Constr ((_p2, {contents = Some (params, ty)}), ts2) ->
          let limit = { limit with expansions = limit.expansions - 1 } in
          match_type pattern (Xtype.subst params ts2 ty) limit
      | _ -> fail

  and match_arrow_types pattern target limit =
    let parrows, preturn = get_core_arrows [] pattern in
    let tarrows, treturn = get_target_arrows  [] target in
    match parrows, tarrows with
    | [], [] -> fail (* avoid inf loop *)
    | _ ->
        match_type preturn treturn limit
        >>= match_types (List.map snd parrows) (List.map snd tarrows)
  
  and remove_target_type_option pattern target limit =
    match target with
(* CR jfuruse: BUG  *)
    | Constr ((Ppath.PPdot (Ppath.PPpredef, "option"), _), [t2]) ->
        decr ~by:10 limit >>= match_type pattern t2
    | _ -> fail
  
  and make_tuple_left pattern target limit =
    match pattern.ptyp_desc, target with
    | _, Tuple ts2 -> match_types [pattern] ts2 limit 
    | _ -> fail
  
  and make_tuple_right pattern target limit =
    match pattern.ptyp_desc, target with
    | Ptyp_tuple ts1, _ -> match_types ts1 [target] limit 
    | _ -> fail
  
  and match_types ts1 ts2 limit =
    (* matching of two list of types, with permutation and addition/removal *)
  
    let len_ts1 = List.length ts1 in
    let len_ts2 = List.length ts2 in
  
    let ts1, ts2, penalty =
      (* Some component might be missing in pattern, we fill variables for them
         but with a rather big price *)
      if len_ts1 < len_ts2 then
        (* CR jfuruse: 1--n is very dangerous. I once wrote [1--n] aiming to produce a list of length n.
           But actually it is 1. *)
        List.map (fun _ -> dummy_core_type_var) (1 -- (len_ts2 - len_ts1)) @ ts1,
        ts2,
        (len_ts2 - len_ts1) * 3
      else if len_ts1 > len_ts2 then
        (* The target can have less components than the pattern,
           but with huge penalty *)
        ts1,
        List.map (fun _ -> dummy_type_expr_var) (1 -- (len_ts1 - len_ts2)) @ ts2,
        (len_ts1 - len_ts2) * 5
      else ts1, ts2, 0
    in

    decr ~by:penalty limit >>= fun limit -> 
  
(*
    let rec fact = function
      | 0 -> 1
      | 1 -> 1
      | n when n > 29 -> max_int
      | n -> fact (n-1) * n
    in
    let size = fact (List.length ts1) in
    if size > 10000 then !!% "Warning: perm: %d %d@." (List.length ts1) size;
*)
  
    (* n^2 *)
    let ts2_array = Array.of_list ts2 in
    let score_table =
      List.map (fun t1 ->
        Array.map (fun t2 ->
          match match_type t1 t2 limit with
          | None -> None
          | Some limit' -> Some (limit.score - limit'.score)
        ) ts2_array) ts1
    in
  
    (* I've got [GtkEnums._get_tables : unit -> t1 * .. * t70].
       Its permutation is ... huge: 1.19e+100.
     *)
  
    let rec perm_max pos xs limit = match xs with
      | [] -> return limit
      | xs ->
          let rec choose sx = function
            | [] -> assert false
            | [x] -> [x, List.rev sx]
            | x::xs -> (x, List.rev_append sx xs) :: choose (x::sx) xs
          in
          let xss = choose [] xs in

          List.fold_left1 max 
          & List.map (fun (x,xs) ->
            match Array.unsafe_get x pos with
            | None -> (* too much cost *) fail
            | Some score ->
                decr ~by:score limit 
                >>= perm_max (pos+1) xs) xss
    in
  
    perm_max 0 score_table limit 
  in
  
  match_type pattern target limit 

(* Return distance, not score *)
let match_path_type (p1, ty1) (p2, ty2) limit_path limit_type =
  let open TypeLimit in
  match_path p1 p2 limit_path; 
  >>= fun _ -> (* Once path test is done, we ignore its score. *)
  match_type ty1 ty2 (create limit_type)
  >>= fun limit -> return (limit_type - limit.score)

(* Return distance, not score *)
let match_type ?no_target_type_instantiate t1 t2 limit_type =
  let open TypeLimit in
  match_type ?no_target_type_instantiate t1 t2 (create limit_type) >>= fun limit -> 
  return (limit_type - limit.score)

(* Return distance, not score *)
let match_path p1 p2 limit =
  match_path p1 p2 limit >>= fun score -> return (limit - score)
