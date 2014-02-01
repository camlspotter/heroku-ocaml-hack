(* eliom item renderer *)

open Spotlib.Spot
open Eliom_content.Html5.D
open ElMisc

open Item

let comment s = span_class "comment" [ !$("(* " ^ s ^ " *)") ]

let path p = div_class "name"
  [ !$ (Xpath.name p)
  ]

let type_expr ?opened ty = 
  let opened = match opened with
    | Some (Ppath.PPdot (p, _)) -> Some (Ppath.to_path_for_printing p)
    | _ -> None
  in
  div_class "type"
    [ !$ (Format.to_string (Xtype.format opened) ty)
    ]

let packages ps =
  div_class "packages" 
    ( !$ "Packages: "
    :: (List.intersperse (!$ ", ")
        & List.map (fun name -> 
          ElServices.Package.a [ !$ name ] name)
        & List.sort compare (List.map OCamlFind.Package.name ps)))

let odoc info =
  match info with
  | `Ok infoopt -> 
      let infostring = 
        match infoopt with
        | None -> ""
        | Some info -> OCamlDoc.info_to_string info
      in
      div_class "info" 
        [ !$ infostring ]
  | `Error _ ->
      div_class "info" [ !$ "ocamldoc failed" ]

let elem_body ps p _loc info desc =
  let space = !$ " " in
  let colon_type ty = 
    div [ span_class "sep" [ !$ ":" ]
        ; space
        ; type_expr ~opened:p ty
        ]
  in
  let equal_type ty = 
    div [ span_class "sep" [ !$ "=" ]
        ; space
        ; type_expr ~opened:p ty
        ]
  in

  let p = Ppath.to_path_for_printing p in 
  let type_params = function
    | [] -> !$ ""
    | [param] -> !$ (Format.to_string (Xtype.format (Some p)) param ^ " " )
    | params -> 
        !$ (Format.to_string (fun ppf params -> 
          Format.fprintf ppf "(%a)" 
            (Format.list ",@," (Xtype.format (Some p))) params) params
          ^ " ")
  in

  let kind s = span_class "kind" [ !$ s] in
  let ent = div_class "entry" in
  match desc with
  | Class -> 
      [ ent [ kind "class"
            ; space
            ; path p
            ]
      ; packages ps
      ; odoc info
      ]
  | ClassType -> 
      [ ent [ kind "class type"
            ; space
            ; path p
            ]
      ; packages ps
      ; odoc info
      ]

  | ClassField (virtual_flag, ty) ->
      [ ent [ kind (match virtual_flag with
                    | Asttypes.Concrete -> "class val"
                    | Asttypes.Virtual  -> "class val virtual")
            ; space
            ; path p
            ; space
            ; colon_type ty
            ]
      ; packages ps
      ; odoc info
      ]
          
  | Constr ty ->
      [ ent [ kind "constr"
            ; space
            ; path p
            ; space
            ; colon_type ty
            ]
      ; packages ps
      ; odoc info
      ]
  | Exception ty ->
      [ ent [ kind "exception"
            ; space
            ; path p
            ; space
            ; colon_type ty
            ]
      ; packages ps
      ; odoc info
      ]

  | Field ty ->
      [ ent [ kind "field"
            ; space
            ; path p
            ; space
            ; colon_type ty
            ]
      ; packages ps
      ; odoc info
      ]
  | Method (private_flag, virtual_flag, ty) ->
      [ ent [ kind (match private_flag, virtual_flag with
                       | Asttypes.Private, Asttypes.Concrete -> "method private"
                       | Asttypes.Private, Asttypes.Virtual  -> "method private virtual"
                       | Asttypes.Public,  Asttypes.Concrete -> "method"
                       | Asttypes.Public,  Asttypes.Virtual  -> "method virtual")
            ; space
            ; path p
            ; space
            ; colon_type ty
            ]
      ; packages ps
      ; odoc info
      ]
  | ModType ->
      [ ent [ kind "module type"
            ; space
            ; path p
            ]
      ; packages ps
      ; odoc info
      ]
  | Module ->
      [ ent [ kind "module"
            ; space
            ; path p
            ]
      ; packages ps
      ; odoc info
      ]
  | Type (params, Some ty, k) ->
      [ ent [ kind "type"
            ; space
            ; type_params params
            ; path p
            ; space
            ; equal_type ty
            ; match k with
              | `Record -> comment "Record"
              | `Variant -> comment "Variant"
              | `Abstract -> !$ ""
        ]
      ; packages ps
      ; odoc info
      ]
  | Type (params, None, k) ->
      [ ent [ kind "type"
            ; space
            ; type_params params
            ; path p
            ; space
            ; match k with
              | `Record -> comment "Record"
              | `Variant -> comment "Variant"
              | `Abstract -> comment "Abstract"
            ]
      ; packages ps
      ; odoc info
      ]
  | Value ty -> 
      [ ent [ kind "val"
            ; space
            ; path p
            ; space
            ; colon_type ty
            ]
      ; packages ps
      ; odoc info
      ]
  | Package (pack, _) ->
      [ ent [ kind "package"
            ; space
            ; path p
            ]
      ; div_class "info" [ !$ (Option.default (OCamlFind.Package.find_var "description" pack) (fun _ -> "no description")) ]
      ]
          
let item (dist, _id, (ps, path, _loc, info, desc : Item.t)) =
  tr [ td_class "distance" [ !$ (string_of_int dist) ]
     ; td_class "entry" 
         ( elem_body ps path _loc info desc )
     ]
