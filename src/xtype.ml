(* Type algebra, independent from type_expr and core_type.

   It is cumbersome, but we should have specif data types for our needs,
   type_expr and core_type could be very memory consuming.
*)

open Spotlib.Spot

module OCaml = Ocaml
module OCaml_conv = Ocaml_conv
(* open OCaml_conv *)

type t = 
  | Link of [ `Linked of t | `Stub ] ref
  | Nil
  | Any
  | VarNamed of int * string (* "" means anonymous *)
  | UnivarNamed of int * string (* "" means anonymous *)
  | Var of int
  | Univar of int
  | Arrow of string * t * t (* We ignore commutatibity *)
  | Tuple of t list
  | Constr of datatype * t list
  | Object of ((string * t) list * [ `Closed | `Open of t]) option 
              * (Ppath.t * t list) option
      (* Named class type: (t,..,t) P.ident

         | Class of Path.t * t list * string list
         for (t,..,t) #P.ident [> name .. name ] 

         #P.ident [> name] is deprecated syntax, so we simply ignore it.
      *)
  | Alias of t * string (* t as 'a *)
  | Variant of xrow
  | Poly of t * t list
  | Package of Ppath.t * (Ppath.t * t) list
      (* S with type M.t = cty and ... *)

(* CR jfuruse: I believe this part can be much simplified *)
and xrow = {
  xrow_fields : (string * [ `Present of t option
                          | `Either of bool * t list 
                          | `Absent ]) list;
  xrow_more : t option;
  xrow_closed : bool;
  xrow_fixed : bool;
  xrow_name : (Ppath.t * t list) option 
}

and datatype = Ppath.t * (t list * t) option ref (* alias *)

type _t = t

module HXTypes = Hashtbl.Make(struct
  type t = _t
  let equal t1 t2 = t1 == t2
  let hash = Hashtbl.hash
end)

let is_recursive ty = 
  let rec f = function
    | Link {contents = `Stub} -> assert false (* not well formed *)
    | Link {contents = `Linked _} -> raise Exit
    | Nil 
    | Any 
    | VarNamed _
    | UnivarNamed _
    | Var _
    | Univar _ -> ()
    | Arrow (_, t1, t2) -> f t1; f t2
    | Tuple ts 
    | Constr (_, ts) -> List.iter f ts
    | Object (fields, names) ->
        ~~ Option.iter fields ~f:(fun (fields, oc) ->
          List.iter (f ** snd) fields;
          match oc with
          | `Open t -> f t
          | `Closed -> ());
        ~~ Option.iter names ~f:(fun pts ->
          List.iter f & snd pts)
    | Alias (t, _) -> f t
    | Poly (t, ts) -> List.iter f (t :: ts)
    | Package (_, pts) -> List.iter (f ** snd) pts
    | Variant xrow ->
        ~~ List.iter xrow.xrow_fields ~f:(fun (_, x) -> match x with
        | `Present (Some t) -> f t
        | `Present None -> ()
        | `Either (_, ts) -> List.iter f ts
        | `Absent -> ());
        Option.iter f xrow.xrow_more;
        Option.iter (List.iter f ** snd) xrow.xrow_name
  in
  try f ty; false with Exit -> true

open Types
open Btype

let to_type_expr pathfix ty =
  (* In one type_expr, id's must be unique *)
  let visited = HXTypes.create 1023 in
  let rec f ty = 
    try HXTypes.find visited ty with Not_found ->
    let link = newgenvar ~name:"link" () in
    HXTypes.replace visited ty link;
    let res = match ty with
      | Link { contents = `Stub } -> assert false
      | Link { contents = `Linked ty } -> Tlink (f ty)
      | Alias _ -> assert false
      | Nil -> Tnil
          
      | Any -> Tvar (Some "_")
      | Var _ -> Tvar None
      | VarNamed (_, s) -> Tvar (Some s)
      | Univar _ -> Tunivar None
      | UnivarNamed (_, s) -> Tunivar (Some s)
      | Arrow (l, Constr ((Ppath.PPdot(Ppath.PPpredef, "option"), _), [ty1]), ty2) when Btype.is_optional l ->
          Tarrow (l, Predef.type_option (f ty1), f ty2, Cok (* ? *))
      | Arrow (l, _, _) when Btype.is_optional l -> 
          assert false
      | Arrow (l, ty1, ty2) -> Tarrow (l, f ty1, f ty2, Cok (* ? *))
      | Tuple tys -> Ttuple (List.map f tys)
      | Constr ((path, _), tys) -> Tconstr (pathfix path, List.map f tys, ref Mnil)
      | Object (None, _) -> assert false
      | Object (Some (fields, opened), nm) ->
          let fields = 
            List.fold_left (fun st (name, ty) ->
              newgenty (Tfield (name, Fpresent, f ty, st)))
              (match opened with
              | `Open ty -> f ty
              | `Closed -> newgenty Tnil)
              fields
          in
          let nm = match nm with
            | None -> None
            | Some (p, typs) ->
                Some (pathfix p, newgenvar () (* dummy: this info is lost but not used for printing *)
                         :: List.map f typs)
          in
          Tobject (fields, ref nm)
      | Poly (t, vars) -> Tpoly (f t, List.map f vars)
      | Package (p, pp_t_list) -> 
          Tpackage (pathfix p, List.map (Ppath.to_longident ** fst) pp_t_list,
                    List.map (f ** snd) pp_t_list)

      | Variant xrow ->
          let row_fields =
            List.sort (fun (l1,_) (l2,_) -> compare l1 l2) 
            &
            List.map (fun (l,fld) -> match fld with
            | `Present topt -> l, Rpresent (Option.map f topt)
            | `Either (const, ts) -> l, Reither (const, List.map f ts, false, ref None)
            | `Absent -> l, Rabsent
            ) xrow.xrow_fields
          in
          let row_more = match xrow.xrow_more with Some t -> f t | None -> Btype.newgenvar () in
          let row_name = Option.map (fun (p,ts) ->
            pathfix p, List.map f ts) xrow.xrow_name 
          in
          Tvariant
          { row_fields;
            row_more;
            row_bound = ();
            row_closed = xrow.xrow_closed;
            row_fixed = xrow.xrow_fixed;
            row_name }
    in
    link.desc <- res;
    link
  in
  f ty

let predefined_types =
  let open Env in
  let rec f st = function
    | Env_type (sum, id, _) -> f (id::st) sum
    | Env_empty -> st
    | Env_value     (sum, _, _)
    | Env_exception (sum, _, _)
    | Env_module    (sum, _, _)
    | Env_modtype   (sum, _, _)
    | Env_class     (sum, _, _)
    | Env_cltype    (sum, _, _)
    | Env_open      (sum, _) -> f st sum
  in
  List.map Ident.name & f [] &  Env.summary Env.initial

let pathfix_for_printing opened p = 
  let open Path in
  match p with
  | Pdot (p, s, _) when opened = Some p && List.mem s predefined_types -> p
  | _ ->
      let rec fix = function
(*
        | Pdot (Pident id, s, _n) when Ident.name id = "{predef}" -> Pident (Ident.create s)
*)
        | Pdot (p, s, _n) when opened = Some p -> Pident (Ident.create s)
        | Pdot (p, s, n) -> Pdot (fix p, s, n)
        | (Pident _ as pid) -> pid
        | Papply (p1, p2) -> Papply(fix p1, fix p2)
      in
      fix p
  
let format opened ppf ty = 
  let ty = to_type_expr (pathfix_for_printing opened ** Ppath.to_path_for_printing) ty in
  Printtyp.reset_and_mark_loops ty; 
  Printtyp.type_expr ppf ty

(* CR jfuruse: Do we need to build new types for each subst?
   We can delay the subst until we see Vars right? *)
(* subst only happens at search so we do not need hcons *)
let subst params types target =
  let bindings = List.combine params types in
  let visited = ref [] in
  let rec subst t = 
    try 
      match !(List.assq t !visited) with
      | `Linked t -> t
      | _ -> assert false
    with Not_found ->
      let res = ref (`Linked Nil) in (* dummy *)
      visited := (t, res) :: !visited;
      let t = match t with
        | Nil | Any | UnivarNamed _ | Univar _ -> t
        | Link {contents = `Linked t} ->
            Link (try List.assq t !visited with Not_found -> assert false)
        | Link {contents = `Stub} -> assert false
        | (Var _ | VarNamed _ as var) ->
            begin try List.assq var bindings
            with Not_found -> var
            end
        | Arrow (lab, t1, t2) ->
            let t1 = subst t1 in
            let t2 = subst t2 in
            Arrow (lab, t1, t2)
        | Tuple ts -> Tuple (List.map subst ts)
        | Constr (dt, ts) -> Constr (dt, List.map subst ts)
        | Object (meths, inherits) ->
            let meths = ~~ Option.map meths ~f:(fun (meths, close_open) ->
              List.map (fun (s,t) -> s, subst t) meths,
              match close_open with
              | `Closed -> `Closed
              | `Open t -> `Open (subst t))
            in
            let inherits = ~~ Option.map inherits ~f:(fun (p, tys) ->
              p, List.map subst tys)
            in
            Object (meths, inherits)
        | Alias _ -> assert false
        | Variant xrow ->
            Variant { xrow with
              xrow_fields = 
                ~~ List.map xrow.xrow_fields ~f:(fun (name, x) ->
                  name, 
                  match x with
                  | `Present topt -> `Present (Option.map subst topt)
                  | `Either (b, ts) -> `Either (b, List.map subst ts)
                  | `Absent -> `Absent);
              xrow_more = Option.map subst xrow.xrow_more;
              xrow_name = ~~ Option.map xrow.xrow_name ~f:(fun (p, ts) ->
                p, List.map subst ts)
            }
        | Poly (t, vars) ->
            List.iter (fun var ->
              match var with
              | UnivarNamed _ | Univar _ -> ()
              | _ -> assert false) vars;
            Poly (subst t, vars)
        | Package (p, constraints) ->
            Package (p, List.map (fun (lid,t) -> lid, subst t) constraints)
      in
      res := `Linked t;
      t
  in
  subst target

module Datatypes = Hashcons.Make(struct
  type t = Ppath.t * (_t list * _t) option ref
  let equal (p1,_) (p2,_) = p1 == p2
  (* CR jfuruse: We ignore the alias information for now *)

  let hash (p, _) = Hashtbl.hash p
end)

module H = Hashcons.Make(struct
  type t = _t
  let hash = Hashtbl.hash

  let forall ts1 ts2 f =
    if List.length ts1 <> List.length ts2 then false
    else List.for_all2 f ts1 ts2

  let option topt1 topt2 f =
    match topt1, topt2 with
    | None, None -> true
    | Some t1, Some t2 -> f t1 t2
    | _ -> false

  let equal t1 t2 = match t1, t2 with
    | Link _, _ | _, Link _ -> false (* loops are not checked *)
    | Nil, Nil
    | Any, Any -> true
    | VarNamed (i,s), VarNamed (i',s') 
    | UnivarNamed (i,s), UnivarNamed (i',s') -> i = i' && s == s' (* string already hconsed *)
    | Var n, Var n'
    | Univar n, Univar n' -> n = n'
    | Arrow (l1, t11, t12), Arrow (l2, t21, t22) ->
        l1 == l2 && t11 == t21 && t12 == t22
    | Tuple ts1, Tuple ts2 -> forall ts1 ts2 (==) 
    | Constr ((p1, _), ts1), Constr ((p2, _), ts2) -> 
        p1 == p2 && forall ts1 ts2 (==) 
    | Object (fields1, names1), Object (fields2, names2) ->
        option fields1 fields2 (fun (fields1, attr1) (fields2, attr2) ->
          forall fields1 fields2 (fun (s1,t1) (s2, t2) -> s1 == s2 && t1 == t2)
          && match attr1, attr2 with
             | `Closed, `Closed -> true
             | `Open t1, `Open t2 -> t1 == t2
             | _ -> false)
(*
        && option names1 names2 (fun (_p1, _ts1) (_p2, _ts2) -> false)
          (* CR jfuruse: TODO *)
          (* #a -> #a is never shared in OCaml. Avoid (#a as 'a) -> 'a *)
*)
        && option names1 names2 (fun (p1, ts1) (p2, ts2) ->
          p1 == p2 && forall ts1 ts2 (==))
    | Alias (t1, s1), Alias (t2, s2) -> t1 == t2 && s1 == s2
    | Poly (t1, ts1), Poly (t2, ts2) -> t1 == t2 && forall ts1 ts2 (==) 
    | Package (p1, lts1), Package (p2, lts2) ->
        p1 == p2 && forall lts1 lts2 (fun (l1, t1) (l2, t2) -> l1 == l2 && t1 == t2) 
    | Variant xrow1, Variant xrow2 -> 
          let field_comp = fun (s1,attr1) (s2,attr2) ->
            s1 == s2
            && match attr1, attr2 with
            | `Present None, `Present None -> true
            | `Present (Some t1), `Present (Some t2) -> t1 == t2
            | `Either (b1, ts1), `Either (b2, ts2) -> b1 = b2 && forall ts1 ts2 (==)
            | `Absent, `Absent -> true
            | _ -> false
          in
        let fields = 
          forall xrow1.xrow_fields xrow2.xrow_fields field_comp
        and more = option xrow1.xrow_more xrow2.xrow_more (==)
        and closed = xrow1.xrow_closed = xrow2.xrow_closed
        and fixed = xrow1.xrow_fixed = xrow2.xrow_fixed
        and name = option xrow1.xrow_name xrow2.xrow_name (fun (p1,ts1) (p2,ts2) ->
          p1 == p2 && forall ts1 ts2 (==))
        in
        let res = fields && more && closed && fixed && name in
        res
(*
        (res |- fun _ ->
          if not res then begin
            !!% "Variant debug @[@[%a@]@ and@ @[%a@]@]@." (format None) t1 (format None) t2;
            !!% "  %b %b %b %b %b@." fields more closed fixed name;
            List.iter2 (fun f1 f2 ->
              if not (field_comp f1 f2) then
                !!% "  ??? %s %b@." (fst f1) (fst f1 == fst f2)) xrow1.xrow_fields xrow2.xrow_fields;
            assert false
          end
         )   
*)
    | _ -> false
end)

(* CR jfuruse: this is incredibly inefficient since it hconsgrep
   all the nodes 
*)
let rec rec_hcons ty = 
  match ty with
  | Link _ -> ty
  | Nil -> ty
  | Any -> ty
  | VarNamed (n, s) -> 
      let s = Hcons.string s in
      H.hcons & VarNamed (n, s)
  | UnivarNamed (n, s) ->
      let s = Hcons.string s in
      H.hcons & UnivarNamed (n, s)
  | Var _ | Univar _ -> H.hcons & ty
  | Arrow (s, t1, t2) ->
      let s = Hcons.string s in
      let t1 = rec_hcons t1 in
      let t2 = rec_hcons t2 in
      H.hcons & Arrow (s, t1, t2)
  | Tuple ts -> 
      let ts = List.map rec_hcons ts in
      H.hcons & Tuple ts
  | Constr (dt, ts) -> 
      let dt = rec_hcons_datatype dt in
      let ts = List.map rec_hcons ts in
      H.hcons & Constr (dt, ts)
  | Object (fields_co_opt, alias_opt) ->
      let fields_co_opt = 
        ~~ Option.map fields_co_opt ~f:(fun (fields, co) ->
          let fields = List.map (fun (s,t) -> Hcons.string s, rec_hcons t) fields in
          let co = match co with
            | `Closed -> `Closed
            | `Open t -> `Open (rec_hcons t)
          in
          fields,co)
      in
      let alias_opt = 
        ~~ Option.map alias_opt ~f:(fun (p, ts) ->
          Ppath.hcons p, List.map rec_hcons ts)
      in
      H.hcons & Object (fields_co_opt, alias_opt)
  | Alias (t, s) -> H.hcons & Alias (rec_hcons t, Hcons.string s)
  | Variant xrow -> H.hcons & Variant (rec_hcons_xrow xrow)
  | Poly (t, ts) -> H.hcons & Poly (rec_hcons t, List.map rec_hcons ts)
  | Package (p, pp_t_lst) ->
      H.hcons & Package (Ppath.hcons p,
                         List.map (fun (p, t) ->
                           (* CR jfuruse: hcons_rec! *)
                           Ppath.hcons p, rec_hcons t) pp_t_lst)

and rec_hcons_xrow xrow =
  let xrow_fields = List.map (fun (s, attr) ->
    Hcons.string s, 
    match attr with
    | `Present topt -> `Present (Option.map rec_hcons topt)
    | `Either (b, ts) -> `Either (b, List.map rec_hcons ts)
    | `Absent -> `Absent)
    xrow.xrow_fields
  in
  let xrow_more = Option.map rec_hcons xrow.xrow_more in
  let xrow_name = Option.map (fun (p, ts) -> Ppath.hcons p, List.map rec_hcons ts) xrow.xrow_name in
  { xrow with xrow_fields; xrow_more; xrow_name }

and rec_hcons_datatype (p,r) = Datatypes.hcons (Ppath.hcons p, r)

let rec_hcons ty =
  let ty' = rec_hcons ty in
  let ty'' = rec_hcons ty' in
  if ty' == ty'' then ty'
  else begin
    !!% "Oops rec_hcons failure %a@." (format None) ty; ty'
  end

open Asttypes
open Parsetree

exception Error

(* This is only for types in search, so no need of hashconsing *) 
let rec of_core_type cty = match cty.ptyp_desc with
  | Ptyp_any -> Any
  | Ptyp_var s -> VarNamed (Hashtbl.hash s (* CR jfuruse: Baad tweeek *), s)
  | Ptyp_arrow (l, cty1, cty2) ->
      let ty1 = of_core_type cty1 in
      let ty2 = of_core_type cty2 in
      Arrow (l, ty1, ty2)
  | Ptyp_tuple ctys -> 
      Tuple (List.map of_core_type ctys)
  | Ptyp_constr ({txt= lid}, ctys) -> 
      Constr ((Ppath.of_longident lid, ref None), List.map of_core_type ctys)
  | Ptyp_object fields ->
      Object (
        Some (
          List.filter_map (function
            | { pfield_desc= Pfield_var } -> None
            | { pfield_desc= Pfield (s, cty) } -> Some (s, of_core_type cty)) fields,
          (if List.exists (function { pfield_desc= Pfield_var } -> true | _ -> false) fields 
           then `Open Nil (* CR jfuruse: strange.. *) else `Closed)),
        None
      )
  | Ptyp_class ({txt=lid}, ctys, []) -> 
      Object (None, Some (Ppath.of_longident lid, List.map of_core_type ctys))
  | Ptyp_class (_, _ctys, _labels) -> assert false
  | Ptyp_alias (cty, s) -> Alias (of_core_type cty, s) 
  | Ptyp_variant (row_fields, closed, labels_opt) -> 
      (* CR jfuruse: We just ignore inherits! *)
      let xrow_fields = List.filter_map (function
        | Rinherit _ -> None
        | Rtag (lab, closed, ctys) ->
            let tl = List.map of_core_type ctys in
            Some (lab, match labels_opt with
              | Some present when not (List.mem lab present) ->
                  `Either(closed, tl)
              | _ ->
                  if List.length tl > 1 || closed && tl <> [] then
                    raise Error;
                  match tl with [] -> `Present None
                  | st :: _ ->
                      `Present (Some st))       ) row_fields
      in
(*
      let inhs = List.filter_map (function
        | Rinherit cty -> Some (of_core_type cty)
        | _ -> None) row_fields
      in
*)
      Variant
      { xrow_fields;
        xrow_more = None;
        xrow_closed = closed;
        xrow_fixed = false; (* ? *)
        xrow_name = None (* ? *) 
      }

  | Ptyp_poly ([], cty) ->  (* object method type like x : int is actually marked as a poly in core_type *)
      of_core_type cty
  | Ptyp_poly (vars, cty) -> 
      Poly (of_core_type cty,
            List.map (fun x -> of_core_type { ptyp_desc= Ptyp_var x;
                                              ptyp_loc = Location.none }) vars) 
  | Ptyp_package ptype ->
      let {txt=lid}, fields = ptype in
      Package (Ppath.of_longident lid,
               List.map (fun ({txt=lid}, cty) -> 
                 Ppath.of_longident lid, of_core_type cty) fields)

open Types
open Btype
open Ctype

let of_type_expr pathconv ty =
  (* In one type_expr, id's must be unique *)
  let visited = Hashtbl.create 1023 in
  let var_cntr = ref 0 in

  let rec f ty = 
    let ty = repr ty in
    try 
      match Hashtbl.find visited ty.id with
      | Link { contents = `Linked ty } -> ty (* It is shared. No loop *)
      | (Link { contents = `Stub } as lty) -> lty (* It is a loop! *)
      | _ -> assert false
    with Not_found ->

    let link = ref `Stub in
    Hashtbl.replace visited ty.id (Link link);
    let res = match ty.desc with
      | Tlink _ -> assert false
      | Tsubst _ -> assert false
      | Tvar None -> incr var_cntr; Var !var_cntr
      | Tvar (Some s) -> incr var_cntr; VarNamed (!var_cntr, Hcons.string s)
      | Tunivar None -> incr var_cntr; Univar !var_cntr
      | Tunivar (Some s) -> incr var_cntr; UnivarNamed (!var_cntr, Hcons.string s)
      | Tarrow (l, ty1, ty2, _) when Btype.is_optional l -> 
          begin match (repr ty1).desc with
          | Tconstr (p, [ty], _) when p = Predef.path_option ->
              Arrow (Hcons.string l, 
                     H.hcons 
                     & Constr (Datatypes.hcons 
                               & Ppath.(hcons 
                                        & PPdot (PPpredef, Hcons.string "option"), 
                                        ref None), 
                               [f ty]), 
                     f ty2)
          | _ -> assert false
          end
      | Tarrow (l, _ty1, _ty2, _) when Btype.is_optional l -> assert false
      | Tarrow (l, ty1, ty2, _) -> Arrow (Hcons.string l, f ty1, f ty2)
      | Ttuple tys -> Tuple (List.map f tys)
      | Tconstr (path, tys, _) -> Constr (Datatypes.hcons (pathconv path, ref None), List.map f tys)
      | Tobject (fi, nm) -> (* of type_expr * (Path.t * type_expr list) option ref *)
          let fields, rest =
            let fields, rest = flatten_fields fi in
            let present_fields =
              List.fold_right (fun (n, k, t) l ->
                match field_kind_repr k with
                | Fpresent -> (Hcons.string n, f t) :: l
                | _ -> l)
                fields [] in
            List.sort (fun (n, _) (n', _) -> compare n n') present_fields,
            rest
          in
          (* let opened = if opened_object ty then `Open else `Closed in *)
          let opened = match (repr rest).desc with
            | Tvar _ | Tunivar _ | Tconstr _ -> `Open (f rest)
            | Tnil -> `Closed
            | _ -> assert false
          in
          let named = match !nm with
            | None -> None
            | Some (p, _ty :: tys) -> Some (pathconv p, List.map f tys)
            | Some (_p, []) -> assert false
          in
          Object (Some (fields, opened), named)
      | Tfield _ -> assert false
      | Tnil -> Nil
      | Tpoly (ty, tyl) -> Poly (f ty, List.map f tyl)
      | Tpackage (path, lids, typs) ->
          Package (pathconv path,
                   List.map2 (fun lid typ ->
                     Ppath.of_longident lid,
                     f typ) lids typs)

      | Tvariant row ->
          let xrow_fields = 
            List.map (fun (l, fld) -> 
              let l = Hcons.string l in
              match fld with
              | Rpresent topt -> (l, `Present (Option.map f topt))
              | Reither (const, ts, _, _) -> (l, `Either (const, List.map f ts))
              | Rabsent -> l, `Absent) 
            & List.sort (fun (l1,_) (l2,_) -> compare l1 l2) row.row_fields
          in
          let xrow_more = Some (f row.row_more) in (* CR jfuruse: always Some? *)
          let xrow_name = Option.map (fun (p,ts) -> pathconv p, List.map f ts) row.row_name in
          Variant
          { xrow_fields;
            xrow_more;
            xrow_closed = row.row_closed;
            xrow_fixed = row.row_fixed;
            xrow_name }
    in
    let res = H.hcons res in
    link := `Linked res;
    res
  in
  f ty

let to_string = Format.to_string (format None)
      
let of_type_expr pathconv ty =
 let pathconv p = 
    let pp = pathconv p in
    let p' = Ppath.to_path_for_debug pp in 
    let pp' = Ppath.of_path p' in
    if pp != pp' then begin
      !!% "@[<2>ASSERT@ @[%a@] and@ @[%a@]@]@."
        (Ocaml.format_with Ppath.ocaml_of_t ) pp
        (Ocaml.format_with Ppath.ocaml_of_t ) pp';
      assert (pp == pp');
    end;
    pp
  in
      
  let xty = of_type_expr pathconv ty in
  let ty' = to_type_expr Ppath.to_path_for_debug xty in

(*
  let pathconv' p = 
    let pp = Ppath.of_path p in
    let p' = Ppath.to_path_for_printing pp in 
    let pp' = Ppath.of_path p' in
    assert (pp == pp');
    pp
  in
*)
  let pathconv' = Ppath.of_path in

  let xty' = of_type_expr pathconv' ty' in
  let sty = to_string xty in
  let sty' = to_string xty' in

  let format_type_expr ppf ty = 
    Printtyp.reset_and_mark_loops ty; 
    Printtyp.type_expr ppf ty 
  in
    
  if xty != xty' && not (is_recursive xty) then begin
    !!% "ERROR hcons @[@[%a@]@ => @[%a@]@]@." 
      format_type_expr ty
      (format None) xty;
    !!% "        and @[@[%a@]@ => @[%a@]@]@." 
      format_type_expr ty'
      (format None) xty';
    assert false
  end else begin
    (* !!% "OK hcons @[@[%a@]@ => @[%a@]@]@." format_type_expr ty (format None) xty; *)
    ()
  end;
  (* CR jfuruse: this is not enough. We somehow compare the printed result
     of ty and ty' *)
  if sty <> sty' then !!% "WARN@.%s@.&@.%s@." sty sty';
  xty
  
let of_type_exprs pathconv tys =
  match of_type_expr pathconv (newgenty(Ttuple tys)) with
  | Tuple tys -> tys
  | _ -> assert false

let to_type_exprs pathfix tys =
  match (to_type_expr pathfix (Tuple tys)).desc with
  | Ttuple tys -> tys
  | _ -> assert false

let () = 
  !!% "Predefined type ids: %s@." & String.concat " " predefined_types

let format_core_type ppf cty = format None ppf & of_core_type cty
