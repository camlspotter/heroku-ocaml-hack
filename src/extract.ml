(**

   The aim of this module is to extract definitions from cmt/cmti files:

   * values
   * constructors
   * record fields
   * types
   * and so on

   During the extraction it also gathers the mapping of the defined idents to their paths
   in the module. For example, if your source code is like:

     module M = struct
       let x = 1
     end

     module N = struct
       type x = int
     end

   then we have a mapping x => M.x and x => N.x (Note that x are actually idents, so
   the first and second x's are nicely distinguished.

*)

open Spotlib.Spot

open Asttypes
open Path
open Typedtree
module Ty = Types
module P = Printtyp

open Item

(* Keep the access paths of defined idents:

   for x in module M = struct let x = ... end,
   x => M.x
*)
let ident_table = Hashtbl.create 1023

let reset_ident_table () = Hashtbl.clear ident_table

let pdot : Path.t -> Ident.t -> Path.t = 
  let open Ident in
  let open Path in
  fun path ident -> Pdot (path, ident.name, ident.stamp)

let add_ident path id =
  let p = pdot path id in
  Hashtbl.replace ident_table (id.Ident.name, id.Ident.stamp) p;
  p
 
(* For module expr functor(A : sig .. end) under P, 
   returns P(A) (the result) and P.A (inside argment) 
*)
let add_functor p id =
  let open Ident in
  let open Path in
  (* I think we need not to remember them in ident_table. *)
  Papply (p, Pident id),
  Pdot (p, id.name, id.stamp)
 
(* exception E of t1 * t2 => t1 -> t2 -> exn *)
let type_of_exception ty_ed =
  List.fold_right (fun ty st ->
    Btype.newgenty (Ty.Tarrow (
      "",
      ty,
      st,
    Ty.Cok))) ty_ed.Types.exn_args Predef.type_exn 

(* type 'a t = C of t1 * t2 => C : t1 -> t2 -> 'a t *)
let type_of_constr tyid type_params tyargs tyopt =
  let ret = Option.default tyopt (fun () -> 
    Btype.newgenty (Ty.Tconstr (Pident tyid, type_params, ref Ty.Mnil)))
  in
  List.fold_right (fun ty st ->
    Btype.newgenty (Ty.Tarrow (
      "",
      ty,
      st,
    Ty.Cok))) tyargs ret

(* type 'a t = { l : tl } => l : 'a t -> tl *)
let type_of_field tyid type_params ty =
  let dom = Btype.newgenty (Ty.Tconstr (Pident tyid, type_params, ref Ty.Mnil)) in
  Btype.newgenty (Ty.Tarrow ("", dom, ty, Ty.Cok))
                             

let rec module_expr path mexp =
  (* let env = mexp.mod_env in *)
  match mexp.mod_desc with
  | Tmod_structure str -> structure path str 
  | Tmod_ident _ -> []
  | Tmod_functor (id, loc, amty, mexp) ->
      let path_result, path_inside = add_functor path id in
      (path_result, loc.loc, Module)
      :: (path_inside, loc.loc, Module)
      :: module_type path_inside amty
      @ module_expr path_result mexp
  | Tmod_apply (mexp1, mexp2, _module_coercion) ->
      (* CR jfuruse: path name is not correct *)
      module_expr path mexp1 
      @ module_expr path mexp2
  | Tmod_constraint (mexp, _ty_mty, module_type_constraint, _module_coercion) ->
      module_constraint 
        (module_expr path mexp)
        (mod_constraint path module_type_constraint)
  | Tmod_unpack (_e, _ty_mty) -> []

and module_constraint _items items_constraint = 
  (* CR jfuruse: We must use the locations obtained from items *)
  items_constraint

and mod_constraint path mtc =
  match mtc with
  | Tmodtype_implicit -> []
  | Tmodtype_explicit mty ->
      module_type path mty

and module_type path mty =
  match mty.mty_desc with
  | Tmty_signature sg -> signature path sg
  | Tmty_ident _ -> []
  | Tmty_functor (id, loc, amty, rmty) -> 
      let path_result, path_inside = add_functor path id in
      (path_result, loc.loc, Module)
      :: (path_inside, loc.loc, Module)
      :: module_type path_inside amty
      @ module_type path_result rmty
  | Tmty_with (mty, _constraints) -> module_type path mty
  | Tmty_typeof mexp -> module_expr path mexp (* CR jfuruse: path is incorrect here *)

and ty_module_type loc path mty =
  let open Types in
  match mty with
  | Mty_signature sg -> ty_signature loc path sg
  | Mty_ident _ -> []
  | Mty_functor (id, amty, rmty) -> 
      let path_result, path_inside = add_functor path id in
      (path_result, loc, Module)
      :: (path_inside, loc, Module)
      :: ty_module_type loc path_inside amty
      @ ty_module_type loc path_result rmty

and type_declaration path loc tyid td =
  let path' = add_ident path tyid in
  (path', loc, Type (td.typ_type.Types.type_params,
                      begin match td.typ_type.Types.type_manifest with
                      | Some ty -> Some ty
                      | None -> None
                      end,
                      match td.typ_kind with 
                      | Ttype_abstract -> `Abstract
                      | Ttype_variant _ -> `Variant
                      | Ttype_record _ -> `Record)) :: 
  match td.typ_kind with
  | Ttype_abstract -> [] (* CR jfuruse: manifest *)
  | Ttype_variant vars (* (Ident.t * string loc * core_type list * Location.t) list *) ->
      (* Typedtree.Ttype_variant misses GADT return type,
         so we try to retrieve it from Types.Type_variant. *)
      List.map2 (fun
        (_id, loc, _tyargs, _loc) (id, tyargs, gadt_tyopt) -> 
            let path = add_ident path id in
            (path,
             loc.loc, 
             Constr (type_of_constr tyid td.typ_type.Types.type_params
                        tyargs gadt_tyopt))) 
        vars
        (match td.typ_type.Types.type_kind with
         | Types.Type_variant vars -> vars
         | _ -> assert false)
        
  | Ttype_record fields
      (* (Ident.t * string loc * mutable_flag * core_type * Location.t) list *) ->
      List.map (fun (id, loc, _, ty, _loc) ->
        let path = add_ident path id in
        (path,
         loc.loc, 
         Field (type_of_field tyid td.typ_type.Types.type_params ty.ctyp_type))) fields

and structure_item : Path.t -> Env.t -> structure_item -> (Path.t * Location.t * _) list
  =  fun path env sitem -> match sitem.str_desc with
  | Tstr_eval _ -> []
  | Tstr_value (_, bindings) -> 
      (* let env = sitem.str_env in *)
      let ids = let_bound_idents bindings in
      List.map (fun id -> 
        let vdesc = Env.find_value (Pident id) env in
        let path = add_ident path id in
        (path, vdesc.Ty.val_loc, Value vdesc.Ty.val_type)) ids
  | Tstr_primitive (id, loc, vd) -> 
      let path = add_ident path id in
      [path, loc.loc, Value vd.val_val.Ty.val_type]
  | Tstr_type decls -> 
      List.concat_map (fun (id, loc, td) ->
        type_declaration path loc.loc id td) decls
  | Tstr_exception (id, loc, ed) -> 
      let path = add_ident path id in
      [path, loc.loc, Exception (type_of_exception ed.exn_exn)]
  | Tstr_exn_rebind (_id, _, _, _) -> []
    (* Ident.t * string loc * Path.t * Longident.t loc *)
  | Tstr_module (id, loc, mexp) ->  
      let path = add_ident path id in
      (path, loc.loc, Module (* (Module_expr mexp) *)) 
      :: module_expr path mexp
  | Tstr_recmodule _defs -> []
  | Tstr_modtype (id, loc, mty (* TODO *)) -> 
      let path = add_ident path id in
      (path, loc.loc, ModType)
      :: module_type path mty
  | Tstr_open _ -> []
  | Tstr_class xs ->
      List.concat_map (fun (cdecl, _, _) -> class_declaration path cdecl) xs
  | Tstr_class_type cltys -> 
      List.concat_map (fun (_id, _loc, cltd) ->
        class_type_declaration path cltd) cltys
  | Tstr_include (mexp, _ids) ->
      ty_module_type sitem.str_loc path mexp.mod_type

and class_declaration path cl_decl =
    let loc = cl_decl.ci_id_name.loc in
    let ids = [ cl_decl.ci_id_class;
                cl_decl.ci_id_class_type;
                cl_decl.ci_id_object;
                cl_decl.ci_id_typesharp ] in
    let path = add_ident path (List.hd ids) in
    List.iter (ignore ** add_ident path) (List.tl ids);
    (path, loc, Class) :: class_expr path cl_decl.ci_expr

and class_type_declaration path cl_decl =
    let loc = cl_decl.ci_id_name.loc in
    let ids = [ cl_decl.ci_id_class;
                cl_decl.ci_id_class_type;
                cl_decl.ci_id_object;
                cl_decl.ci_id_typesharp ] in
    let path = add_ident path (List.hd ids) in
    List.iter (ignore ** add_ident path) (List.tl ids);
    (path, loc, ClassType) :: class_type path cl_decl.ci_expr

and class_type_description path cl_decl =
    (* same as class_type_declaration. Only diff is to create Class,
       not ClassType *)
    let loc = cl_decl.ci_id_name.loc in
    let ids = [ cl_decl.ci_id_class;
                cl_decl.ci_id_class_type;
                cl_decl.ci_id_object;
                cl_decl.ci_id_typesharp ] in
    let path = add_ident path (List.hd ids) in
    List.iter (ignore ** add_ident path) (List.tl ids);
    (path, loc, Class) :: class_type path cl_decl.ci_expr

and class_type path clty =
    match clty.cltyp_desc with
    | Tcty_constr _ (* constraint *) -> []
    | Tcty_fun (_, _, clty) ->
        (* CR jfuruse: this should be remembered as class creation
           function ? *)
        class_type path clty
    | Tcty_signature csig -> class_signature path csig

and class_signature path csig =
    List.concat_map (class_type_field path) csig.csig_fields

and class_type_field path ctfield =
    let loc = ctfield.ctf_loc in
    match ctfield.ctf_desc with
    | Tctf_inher _ -> 
        (* CR jfuruse: So it misses virt methods? *)
        (* CR jfuruse: TODO *)
        []
    | Tctf_val (name, _mutable_flag, virtual_flag, core_type) ->
        (* CR jfuruse: mutable flag is thrown away *)
        let id = Ident.create_persistent name in
        let path = add_ident path id in
        [ path, loc, ClassField (virtual_flag, core_type.ctyp_type) ]
    | Tctf_virt (name, private_flag, core_type) ->
        let id = Ident.create_persistent name in
        let path = add_ident path id in
        [ path, loc, Method (private_flag, Virtual, core_type.ctyp_type) ]
    | Tctf_meth (name, private_flag, core_type) ->
        let id = Ident.create_persistent name in
        let path = add_ident path id in
        [ path, loc, Method (private_flag, Concrete, core_type.ctyp_type) ]
    | Tctf_cstr _ -> [] (* CR jfuruse: constraint? *)

and class_expr path clexpr = match clexpr.cl_desc with
  | Tcl_ident (_, _, _) -> []
  | Tcl_structure clstr -> class_structure path clstr
  | Tcl_fun (_, _, _, clexpr, _) -> class_expr path clexpr
  | Tcl_apply (clexpr, _) -> class_expr path clexpr
  | Tcl_let (_, _, _, clexpr) -> class_expr path clexpr
  | Tcl_constraint (clexpr, 
                    _ (* class_type option *) (* CR jfuruse: required? *),
                    _, _, _concr) -> class_expr path clexpr

and class_structure path clstr = 
    List.concat_map (class_field path) clstr.cstr_fields

and class_field path clfield =
    match clfield.cf_desc with
    | Tcf_inher (_, _clexpr, _self, _fields1, _fields2) ->
        (* let loc = clfield.cf_loc in *)
        (* Inherited instance variables and concrete methods *)
        (* CR jfuruse: So it misses virt methods? *)
        (* CR jfuruse: TODO *)
        []

    | Tcf_val (_name, loc, _, id, class_field_kind, _override) ->
        let path = add_ident path id in
        let ty, virtual_ = match class_field_kind with
          | Tcfk_virtual cty -> cty.ctyp_type, Virtual
          | Tcfk_concrete expr -> expr.exp_type, Concrete
        in
        [path, loc.loc, ClassField (virtual_, ty)] 
    | Tcf_meth (name, loc, private_, class_field_kind, _override) ->
        let path = add_ident path (Ident.create name) (* CR jfuruse: ? *) in
        let ty, virtual_ = match class_field_kind with
          | Tcfk_virtual cty -> cty.ctyp_type, Virtual
          | Tcfk_concrete expr -> expr.exp_type, Concrete
        in
(*
        (* ty contains class in the argument *)
*)
        [path, loc.loc, Method (private_, virtual_, ty)] 
    | Tcf_constr _ -> []
    | Tcf_init _ -> []


and signature_item path sgitem = match sgitem.sig_desc with
  | Tsig_value (id, loc, vd) -> 
      let path = add_ident path id in
      [path, loc.loc, Value vd.val_val.Ty.val_type]
  | Tsig_type type_decls ->
      List.concat_map (fun (id, loc, tdecl) ->
        type_declaration path loc.loc id tdecl
      ) type_decls 
  | Tsig_exception (id, loc, ed) -> 
      let path = add_ident path id in 
      [path, loc.loc, Exception (type_of_exception ed.exn_exn)]
  | Tsig_module (id, loc, mty) -> 
      let path = add_ident path id in
      (path, loc.loc,Module (* (Module_type mty) *))
      :: module_type path mty

  | Tsig_recmodule xs ->
      List.concat_map (fun (id, loc, mty) ->
        let path = add_ident path id in
        (path, loc.loc,Module (* (Module_type mty) *))
        :: module_type path mty) xs

  | Tsig_modtype (id, loc, mtyd) -> 
      let path = add_ident path id in
      (path, loc.loc, 
       ModType (* (match mtyd with 
                 | Tmodtype_abstract -> `Abstract 
                 | Tmodtype_manifest mty -> `Module_type mty) *))
       :: (match mtyd with
           | Tmodtype_abstract -> []
           | Tmodtype_manifest mty -> module_type path mty)

  | Tsig_open (_override, _path, _loc) -> []
  | Tsig_include (_mty, ty_sig) -> 
      ty_signature sgitem.sig_loc path ty_sig

  | Tsig_class clds ->
      List.concat_map (class_type_description path) clds

  | Tsig_class_type cltys ->
      List.concat_map (class_type_declaration path) cltys

and ty_signature_item loc path sgitem = 
  let open Types in
  match sgitem with
  | Sig_value (id, vd) -> 
      let path = add_ident path id in
      [path, loc, Value vd.val_type]
  | Sig_exception (id, ty_ed) -> 
      let path = add_ident path id in 
      [path, loc, Exception (type_of_exception ty_ed)]
  | Sig_module (id, mty, _) -> 
      let path = add_ident path id in
      (path, loc, Module (* (`Ty_module_type mty) *))
      :: ty_module_type loc path mty
  | Sig_modtype (id, mtyd) ->
      let path = add_ident path id in
      (path, loc, 
       ModType (* (match mtyd with
                   | Modtype_abstract -> `Abstract
                   | Modtype_manifest mty -> `Ty_module_type mty) *))
      :: (match mtyd with
         | Modtype_abstract -> []
         | Modtype_manifest mty -> ty_module_type loc path mty)
  | Sig_type (id,ty_tydecl, _) -> ty_type_declaration loc path id ty_tydecl
  (* CR jfuruse: todo *)
  | (Sig_class (_, _, _)|Sig_class_type (_, _, _)) -> []


and structure path str =
  (* cache reset may required *)
  let env = Envaux.env_of_only_summary str.str_final_env in
  List.concat_map (structure_item path env) str.str_items

and signature path sg =
  List.concat_map (signature_item path) sg.sig_items

and ty_signature loc path sg =
    List.concat_map (ty_signature_item loc path) sg

and ty_type_declaration loc path tyid td = 
  let open Types in
  let path' = add_ident path tyid in
  (path', loc, Type (td.Types.type_params,
                      begin match td.type_manifest with
                      | Some ty -> Some ty
                      | None -> None
                      end,
                      match td.type_kind with
                      | Type_abstract -> `Abstract
                      | Type_variant _ -> `Variant
                      | Type_record _ -> `Record)) ::
  match td.type_kind with
  | Type_abstract -> [] (* CR jfuruse: manifest *)
  | Type_variant vars (* (Ident.t * type_expr list * type_expr option) list *) ->
      List.map (function
        | (id, tyargs, tyopt (* gadt return *)) -> 
            let path = add_ident path id in
            (path,
             loc,
             Constr (type_of_constr tyid td.type_params tyargs tyopt))
      ) vars

  | Type_record (fields, _rec_repr) -> (*  (Ident.t * mutable_flag * type_expr) list * record_representation *)
      List.map (fun (id, _mut, ty) -> (* CR jfuruse: mutables *)
        let path = add_ident path id in
        (path,
         loc,
         Field (type_of_field tyid td.type_params ty))) fields

let structure path str = 
  reset_ident_table ();
  let res = structure path str in
  res, Hashtbl.copy ident_table

let signature path sg =
  reset_ident_table ();
  let res = signature path sg in
  res, Hashtbl.copy ident_table

let reset_envs () = 
  Env.reset_cache ();
  reset_ident_table ();
  Envaux.reset_cache ();
  Cmfile.reset_cmi_file_cache ()

