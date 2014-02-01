open Spotlib.Spot
open Orakuda.Regexp.Infix

open Item

let () = Lexer.init ()

(* CR jfuruse: clean up required *)
type kind = [ `Class 
             | `ClassField
             | `ClassType
             | `Constr
             | `Exception
             | `Field
             | `Method
             | `ModType
             | `Module
             | `Type
             | `Value 
             | `Package 
             | `ExactPackage ]
type t = 
    { kind : kind option;
      path : Longident.t option;
      type_ : Parsetree.core_type option }

module Parse = struct
  open Parsetree

  (* CR jfuruse: it does not parse _, M._ or M._.x *)
  (* CR jfuruse: it does not parse Make : _  (make : _ works) *)
  let path str =
    try
      let sitem = XParser.pattern_longident Lexer.token
        & Lexing.from_string str
      in 
      Some { kind = None; path= Some sitem; type_= None }
    with
    | _ -> !!% "failed to parse as a path %S@." str; None

  let type_ str = 
    try
      let sg = 
        Parser.interface Lexer.token
          & Lexing.from_string & "val x : " ^ str
      in
      match sg with
      | [{ psig_desc = Psig_value (_, vdesc) }] -> 
          let ty = vdesc.pval_type in
          Some { kind = None; path = None; type_= Some ty }
      | _ -> None
    with
    | _ -> None

  let path_type str =
    !!% "path_type %S@." str;
    try
      let pos = String.index str ':' in
      let p = String.sub str 0 pos in
      let t = String.sub str (pos+1) (String.length str - pos - 1) in
      let p = path p in
      let t = type_ t in
      match p, t with
      | None, _ | _, None -> None
      | Some p, Some t ->
          Some { kind= None; path= p.path; type_= t.type_ }
(*
      | Some {path = Some Lident ("_" | "_*_")}, 
        Some {type_ = Some { ptyp_desc= Ptyp_any }} -> 
          (* Funny *)
*)
    with
    | _ -> None

  let parse_kind s =
    let s = <:s<\s+/ /g>> s in
    match s with
    | "class"       -> Some `Class
    | "class val"   -> Some `ClassField
    | "class type"  -> Some `ClassType
    | "constr"      -> Some `Constr
    | "exception"   -> Some `Exception
    | "field"       -> Some `Field
    | "method"      -> Some `Method
    | "module type" -> Some `ModType
    | "module"      -> Some `Module
    | "type"        -> Some `Type
    | "val"         -> Some `Value
    | "package"     -> Some `Package
    | _ -> None

  let prefixed str =
    let open Option.Open in
    try
      (str =~ <:m<(class|class\s+val|class\s+type|constr|exception|field|method|module|module\s+type|type|val|package)\s+>>) >>= fun res ->
      parse_kind res#_1 >>= fun k ->
      path res#_right >>= fun p ->
      return { kind=Some k; path=p.path; type_= None }
    with
    | _ -> None

  let prefixed_and_type str =
    let open Option.Open in
    try
      (str =~ <:m<(class\s+val|constr|exception|field|method|val)\s+>>) >>= fun res ->
      parse_kind res#_1 >>= fun k -> 
      path_type res#_right >>= fun res ->
      return { res with kind = Some k }
    with
    | _ -> None

  let query s = [ path_type s ; path s ; type_ s; prefixed s; prefixed_and_type s ]

end

let kind_to_string = function
  | `Class -> "class"
  | `ClassField -> "class val" 
  | `ClassType -> "class type" 
  | `Constr -> "constr" 
  | `Exception -> "exception"
  | `Field -> "field"
  | `Method -> "method"
  | `ModType -> "module type"
  | `Module -> "module"
  | `Type -> "type"
  | `Value -> "val"
  | `Package -> "package"
  | `ExactPackage -> "exact_package"
  
let string_of = function
  | { kind= k;
      path= popt;
      type_= tyopt } ->
      begin match k with
      | None -> "*"
      | Some k -> kind_to_string k
      end ^ " " ^
      begin match popt with
      | None -> "*"
      | Some p -> Xlongident.name p (* CR jfuruse: this is the only place we use Xlongident *)
      end ^ " : " ^
      begin match tyopt with
      | None -> "_"
      | Some ty -> Format.to_string Xtype.format_core_type ty
      end

let query items qs =

  let full_query max_dist {kind= k_opt; path= lident_opt; type_= qtyp_opt} = 
    fun (_packs, path, _loc, _infoopt, k') ->
      match k' with
      | Class 
      | ClassType
      | ModType
      | Module
      | Type _ 
      | Package _ ->
          begin match k_opt, k' with
          | (None | Some `Class), Class 
          | (None | Some `ModType), ModType
          | (None | Some `Module), Module
          | (None | Some `Type), Type _
          | (None | Some `ClassType), ClassType
          | (None | Some `Package), Package _ ->
              begin match lident_opt, qtyp_opt with
              | None, None -> Some 10
              | Some lid, None -> 
                  Match.match_path lid path 10 
              | _, Some _ -> None
              end
          | Some `ExactPackage, Package _ ->
              begin match lident_opt, qtyp_opt with
              | None, None -> None
              | Some lid, None -> 
                  Match.match_path lid path 0 
              | _, Some _ -> None
              end
          | _ -> None
          end
      | ClassField _
      | Constr _
      | Exception _
      | Field _
      | Method _
      | Value _ -> 
          begin match k_opt, k' with
          | (None | Some `ClassField) , ClassField (_, typ)
          | (None | Some `Constr    ) , Constr typ
          | (None | Some `Exception ) , Exception typ
          | (None | Some `Field     ) , Field typ
          | (None | Some `Method    ) , Method (_, _, typ)
          | (None | Some `Value     ) , Value typ ->
              begin match lident_opt, qtyp_opt with
              | None, None -> Some 10
              | Some lid, None -> Match.match_path lid path 10 
              | None, Some qtyp -> Match.match_type qtyp typ max_dist 
              | Some lid, Some qtyp ->
                  Match.match_path_type (lid,qtyp) (path,typ) 10 max_dist 
              end
          | _ -> None
          end
  in

  (* Only sort by name, but it can be costy *)
  let group_scored_items (score,items) =
    let open Ppath in
    (* group by path postfix *)
    let rec postfix = function
      | PPident id -> id
      | PPdot (_, name) -> name
      | PPapply (p1, _) -> postfix p1 (* ? *)
      | PPpack _ -> assert false
      | PPpredef -> assert false
    in
    let rec len = function (* rough. (+) is counted as 1 *)
      | PPdot (PPpredef, s) -> String.length s
      | PPident id -> 
          begin match id with
          | "{stdlib}" -> 0 (* CR jfuruse: ? *)
          | "Pervasives" -> 0
          | s -> String.length s
          end
      | PPdot (p, name) -> len p + String.length name + 1
      | PPapply (p1, p2) -> len p1 + len p2 + 3
      | PPpack pks -> String.length (OCamlFind.Packages.to_string pks) + 2
      | PPpredef -> 0
    in
    List.map (fun (i,j) -> (score,i,j))
    & List.concat 
    & List.map (
      (* inside a group, sort by the length of the name *)
      List.sort (fun (_, (_packs1, p1, _, _, _)) (_, (_packs2, p2, _, _, _)) ->
        compare (len p1) (len p2)
      ))
    & List.sort_then_group (fun (_, (_packs1, p1, _, _, _)) (_, (_packs2, p2, _, _, _)) -> 
      compare (postfix p1) (postfix p2)) items
  in

  let funny = function
    | { kind=_; path=None; type_=None } -> true
    | _ -> false
  in

  match qs with
  | [] -> `EmptyQuery
  | _ when List.for_all (function None -> true | _ -> false) qs -> `Error
  | _ when List.for_all (function None -> true
                                | Some s -> funny s) qs -> `Funny
  | _ ->
      let qs = List.filter_map (function
        | None -> None
        | Some q when funny q -> None
        | Some q -> Some q) qs 
      in

      (* This is bad, since '_' lists things twice! and the stack was overflown  *)
      let found, search_time = Unix.timed (fun () ->
        (* None < Some x *)
        Array.foldi_left (fun st i item ->
          let dist = 
            (* None < Some x *) 
            List.fold_left max None 
            & List.map (fun q -> full_query 30 q item) qs
          in
          match dist with
          | Some dist -> Distthresh.add st dist (i, item)
          | None -> st) (Distthresh.create ~thresh:30 ~limit:200) items) ()
      in
      let final_result, group_time = Unix.timed (fun () ->
        List.TR.concat_map group_scored_items
        & Distthresh.to_list found) ()
      in
      `Ok (qs, final_result, search_time, group_time)

let search items query_string = query items & Parse.query query_string

let textual_query items query_string =
  match search items query_string with
  | `Ok (_qs, res, search_time, group_time) -> 
      !!% "%f secs (search: %f, gruop: %f)@." (search_time +. group_time) search_time group_time;
      List.iter (fun (score, _id, result) -> !!% "%d : %a@." score Item.format result) res
  | `Error -> !!% "The input could not be parsed as a query@."
  | `Funny -> !!% "Hahaha! You are so funny. _:_@."
  | `EmptyQuery -> ()

let rec cui items =
  print_string "? "; 
  flush stdout;

  textual_query items & read_line ();
  cui items

let cli () =
  match Conf.files with
  | [] -> 
      if Conf.dump then Load.dump_items (); 
      let { Load.items } = Load.load_items () in
      cui items
  | _ ->
(*
      let package_of ~file_path:_ = None in (* CR jfuruse: correct? *)
      List.iter (fun file -> 
        List.iter (!!% "%a@." Item.format)
        & Load.load [] package_of file) Conf.files
*)
      assert false

