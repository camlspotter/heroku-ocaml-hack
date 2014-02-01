open Spotlib.Spot
open Ppath

let add_pack_name find_packages local_tbl ocaml_root_path = 
  let add_pack_name_for_ident { Ident.name = id; stamp } =
    try
      (* Local path names are replaced using the history *)
      let path = Ppath.of_path & Hashtbl.find local_tbl (id, stamp) in
      match path with
      | PPident _ -> assert false (* All the ident must be prefixed with the top module name *)
      | _ -> path
    with
    | Not_found -> 
        let id = Hcons.string id in
        if stamp > 0 && stamp < 1000 then PPdot (PPpredef, id)
        else if stamp = 0 then
          (* I guess it is global, so module name.
             Search the OCamlFind package name of the module.
          *)
          match find_packages id with
          | None -> PPdot (Ppath.hcons & PPident "???", id) (* no hcons required *)
          | Some pack_ml_path_list -> 
              (* ml_path may be different each other... 
                 We must choose something best *)
              let packs, path = 
                List.fold_left (fun st (pack, path) ->
                  match st with
                  | None -> Some ([pack], path)
                  | Some (_packs, path') when List.length path > List.length path' ->
                      Some ([pack], path)
                  | Some (packs, path') when path = path' ->
                      Some ((pack ::packs), path)
                  | _ ->
                      (* CR jfuruse: We choose the first we see for now *)
                      st) None pack_ml_path_list
                |> from_Some
              in
              let ppacks = Ppath.hcons & PPpack (Packageshack.of_packages packs) in
              let rec build st = function
                | [] -> st
                | x::xs -> build (Ppath.hcons & PPdot (st, Hcons.string x)) xs
              in
              build ppacks path
        else 
          (* The ident is not scanned at load, due to
             "not-implemented" *) 
          PPdot (Ppath.hcons & PPdot (Ppath.of_path ocaml_root_path, "..."), id)
  in
  let open Path in
  let rec add_pack_name = function
    | Pdot (t, name, _) -> Ppath.hcons & PPdot (add_pack_name t, Hcons.string name)
    | Papply (t1, t2) -> Ppath.hcons & PPapply (add_pack_name t1, add_pack_name t2)
    | Pident id when (Ident.name id).[0] = '{' -> 
        (* It is an ident made from a package name *)
        let name = Ident.name id in
        PPpack (OCamlFind.Packages.of_string & String.(sub name 1 & length name - 2))
    | Pident id -> Ppath.hcons & add_pack_name_for_ident id
  in
  add_pack_name

(* CR jfuruse: this one and the above add_pack_name are independent each other *)
module FromResult = struct

  open Item

  let convert' pathconv desc =
    let tconv = Xtype.of_type_expr pathconv in
    let tconvs = Xtype.of_type_exprs pathconv in
    match desc with
    | Constr ty -> Constr (tconv ty)
    | Exception ty -> Exception (tconv ty)
    | Field ty -> Field (tconv ty)
    | Module -> Module
    | ModType -> ModType
    | ClassType -> ClassType
    | Type (params, None, k) -> 
        let params = tconvs params in
        Type (params, None, k)
    | Type (params, Some typ, k) -> 
        begin match tconvs (typ :: params) with
        | typ :: params ->
            Type (params, Some typ, k)
        | _ -> assert false
        end
    | Value ty -> Value (tconv ty)
    | Method (p, v, ty) -> Method (p, v, tconv ty)
    | ClassField (v, ty) -> ClassField (v, tconv ty)
    | Class -> Class
    | Package (p, paths) -> Package (p, paths)
        (* Package is created outside of Pathfix, so no need to touch it *)

  let convert pathconv results =
    (* !!% "Pathfix.convert %d elemes@." (List.length results); *)
    List.map (fun (p, loc, odoc, k) -> 
      let p = Ppath.hcons p in
      p, loc, odoc, convert' pathconv k) results
    (* |- fun _ -> !!% "Pathfix.convert done@." *)


end


let test () = 
  let ty = Predef.type_int in
  let xty = Xtype.of_type_expr (add_pack_name (fun _ -> assert false) (Hashtbl.create 1) Predef.path_int (* dummy *)) ty in
  let ty' = Xtype.to_type_expr Ppath.to_path_for_debug xty in
  let xty' = Xtype.of_type_expr Ppath.of_path ty' in
  assert (xty == xty')
  
let () = test ()
