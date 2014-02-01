(* CR jfuruse: Elem or Item ?*)
open Spotlib.Spot
(* open Asttypes *)
open Tiny_json
(* open Json_conv *)

type virtual_flag = Asttypes.virtual_flag = Virtual | Concrete with conv(json)
type private_flag = Asttypes.private_flag = Private | Public with conv(json)

type 'typ kind = 
  | Class
  | ClassType
  | ClassField of virtual_flag * 'typ
  | Constr of 'typ
  | Exception of 'typ
  | Field of 'typ
  | Method of private_flag * virtual_flag * 'typ
  | ModType
  | Module
  | Type of 'typ list (** type params *) * 'typ option * [ `Abstract | `Record | `Variant ]
  | Value of 'typ
  | Package of OCamlFind.Package.t * string list (** Ex. [ "Dbm" ] *)
(* with conv(json) *)

type t =
    OCamlFind.Package.t list
    * Ppath.t
    * Location.t
    * (Odoc_info.info option, OCamlDoc.error) Result.t
    * Xtype.t kind
(* with conv(json) *)

(* do not hcons itself *)
let rec_hcons_k p = function
  | ClassField (vf, ty) -> ClassField (vf, Xtype.rec_hcons ty)
  | Constr ty -> Constr (Xtype.rec_hcons ty)
  | Exception ty -> Exception (Xtype.rec_hcons ty)
  | Field ty -> Field (Xtype.rec_hcons ty)
  | Method (pf, vf, ty) -> Method (pf, vf, Xtype.rec_hcons ty)
  | Type (tys, tyopt, attr) -> 
      let tys = List.map Xtype.rec_hcons tys in
      let tyopt = Option.map Xtype.rec_hcons tyopt in
      let (_p', alias) = Xtype.rec_hcons_datatype (Ppath.hcons p, ref None (* dummy *)) in
      alias := begin match tyopt with
      | None -> None
      | Some ty -> Some (tys, ty)
      end;
      Type (tys, tyopt, attr)
  | Value ty -> Value (Xtype.rec_hcons ty)
  | (Class | ClassType | ModType | Module as k) -> k
  | Package (p, paths) -> Package (p, List.map Hcons.string paths)

let rec_hcons (ps, p, loc, docopt, k) =
  let p = Ppath.hcons p in
  ps,
  p,
  Hcons.location loc,
  docopt,
  rec_hcons_k p k

let format ppf (packages, path, loc, info, desc : t)=
  let open Format in
  let format_xtype =
    let opened = match path with
      | Ppath.PPdot (p, _) -> Some p
      | _ -> None
    in
    Xtype.format (Option.map Ppath.to_path_for_debug opened)
  in
  let format_packages ppf ps =
    fprintf ppf "@[<v2>Packages:@ @[%a@]@]"
      (Format.list ",@ " (fun ppf p -> Format.string ppf p.OCamlFind.Package.name))
      ps
  in
  let format_doc ppf info = string ppf & OCamlDoc.info_to_string info in
  let format_doc ppf = function
    | `Ok None -> ()
    | `Ok (Some info) -> format_doc ppf info
    | `Error _ -> fprintf ppf "ocamldoc failed"
  in
  match desc with
  | Value ty ->
      fprintf ppf  "@[<v>%a:@ @[<v>val %a@ : @[%a@]@]@ %a@ %a@]@."
        Location.print_loc loc
        Ppath.format path
        format_xtype ty
        format_packages packages
        format_doc info

  | Exception ty ->
      fprintf ppf  "@[<v>%a:@ @[<v>exception %a@ : @[%a@]@]@ %a@ %a@]@."
        Location.print_loc loc
        Ppath.format path
        format_xtype ty
        format_packages packages
        format_doc info

  | Module ->
      fprintf ppf  "@[<v>%a:@ @[<v>module %a@]@ %a@ %a@]@."
        Location.print_loc loc
        Ppath.format path
        format_packages packages
        format_doc info

  | ModType ->
      fprintf ppf "@[<v>%a:@ @[<v>module type %a@]@ %a@ %a@]@."
        Location.print_loc loc
        Ppath.format path
        format_packages packages
        format_doc info

  | ClassType ->
      fprintf ppf  "@[<v>%a:@ @[<v>class type %a@]@ %a@ %a@]@."
        Location.print_loc loc
        Ppath.format path
        format_packages packages
        format_doc info

  | Type (params, tyop, k) ->
      fprintf ppf "@[<v>%a:@ @[<v>type %a%a %t@]@ %a@ %a@]@."
        Location.print_loc loc
        (fun ppf -> function
          | [] -> ()
          | [param] -> format_xtype ppf param
          | params ->
              fprintf ppf "(@[%a@]) "
                (Format.list ",@," format_xtype) params) params
        Ppath.format path
        (fun ppf ->
          match tyop, k with
          | None, `Abstract -> pp_print_string ppf "(* abstract *)"
          | None, `Record -> pp_print_string ppf "= { .. }"
          | None, `Variant -> pp_print_string ppf "= .. | .."
          | Some ty, `Abstract ->
              fprintf ppf "= @[%a@]" format_xtype ty
          | Some ty, `Record ->
              fprintf ppf "= { .. } = @[%a@]" format_xtype ty
          | Some ty, `Variant ->
              fprintf ppf "= .. | .. = @[%a@]" format_xtype ty)
        format_packages packages
        format_doc info

  | Constr ty ->
      fprintf ppf  "@[<v>%a:@ @[<v>constr %a@ : @[%a@]@]@ %a@ %a@]@."
        Location.print_loc loc
        Ppath.format path
        format_xtype ty
        format_packages packages
        format_doc info

  | Field ty ->
      fprintf ppf  "@[<v>%a:@ @[<v>field %a@ : @[%a@]@]@ %a@ %a@]@."
        Location.print_loc loc
        Ppath.format path
        format_xtype ty
        format_packages packages
        format_doc info

  | Class ->
      fprintf ppf  "@[<v>%a:@ @[<v>class %a@]@ %a@ %a@]@."
        Location.print_loc loc
        Ppath.format path
        format_packages packages
        format_doc info

  | ClassField (v, ty) ->
      fprintf ppf  "@[<v>%a:@ @[<v>class val%s %a : %a@]@ %a@ %a@]@."
        Location.print_loc loc
        (match v with Virtual -> " virtual" | Concrete -> "")
        Ppath.format path
        format_xtype ty
        format_packages packages
        format_doc info

  | Method (p, v, ty) ->
      fprintf ppf  "@[<v>%a:@ @[<v>method%s %a : %a@]@ %a@ %a@]@."
        Location.print_loc loc
        (match p, v with
        | Private,  Virtual -> " private virtual"
        | Private,  Concrete -> " private"
        | Public,  Virtual -> " virtual"
        | Public,  Concrete -> ""
        )
        Ppath.format path
        format_xtype ty
        format_packages packages
        format_doc info

  | Package (_p, _mods) -> (* CR jfuruse: todo *)
      fprintf ppf  "@[<v>%a:@ @[<v>package %a@]@]@."
        Location.print_loc loc
        Ppath.format path
