(* Display info of an OCamlFind package *)

open Spotlib.Spot
open Eliom_content.Html5.D (* provides functions to create HTML nodes *)
open ElMisc

module OCP = OCamlFind.Package

let service = ElServices.Package.service

let opam_link p = 
  if p.OPAM.base then !$ (OPAM.name_of_package p)
  else
    let service = Eliom_service.external_service
      ~prefix:"http://opam.ocaml.org"
      ~path: [ "pkg"; p.OPAM.name ; !% "%s.%s" p.OPAM.name p.OPAM.version ]
      (* ~keep_nl_params: .. *)
      ~get_params:Eliom_parameter.unit
      ()
    in
    a ~service [ !$ (OPAM.name_of_package p) ] ()

let notfound name = 
  [ p_class "status" [ !$% "%S: no such OCamlFind package in DB" name ]
  ]

let ambiguous name = 
  [ p_class "status" [ !$% "%S: the query is ambiguous" name ]
  ]

let package search_time group_time pack mpaths =
  let opam = 
    let top_ocamlfind_name = <:s<\..*/>> pack.OCP.name in
    ~~ List.find_map_opt ElLoad.ocamlfind_opam_table ~f:(fun (p, opam) ->
      if p.OCP.name = top_ocamlfind_name then Some opam
      else None)
  in
  let find_var k = match OCP.find_var k pack with
    | None -> "none"
    | Some s -> s
  in
  let name = pack.OCP.name in
  let version = find_var "version" in
  [ p_class "status" [ !$% "(%0.4f secs) (%0.4f secs for search, %0.4f secs for grouping)" (search_time +. group_time) search_time group_time]

  ; 
    let trs = 
       [ tr [ td_class "package_left" [ !$ "OCamlFind Package:" ]
            ; td [ span_class "package" [ !$ name ]
                 ; !$% " (%s)" version
                 ]
            ]

       ; tr [ td [ !$ "OPAM package:" ]
            ; td [ match opam with 
                     | None -> !$ "<not found>"
                     | Some None -> !$ "<unknown>"
                     | Some (Some p) -> opam_link p ]
            ]
         
       ; tr [ td [ !$ "Description:" ]
            ; td [ !$ (find_var "description") ]
            ]

       ; tr [ td [ !$ "Requires:" ]
            ; td & match OCP.requires pack with
                   | Some [] -> [ !$ "none" ]
                   | None -> [ !$ "none" ] (* CamlIDL has no Requires: and it needs noting. *)
                   | Some packs -> 
                       List.intersperse !$" "
                       & List.map (fun pack ->
                         ElServices.Package.a [ !$ pack ] pack) packs
            ]

       ; tr [ td [ !$ "Modules:" ]
            ; td & List.intersperse (!$ " ")
                     (List.map (fun p -> !$ p) mpaths)
            ]
       ]
    in
    table (List.hd trs) (List.tl trs)

  ]


let query name = 
(*
  let lid = Longident.Lident ("{" ^ name ^ "}") in
*)
  let lid = Longident.Lident name in
  let res = 
    match
      Query.query ElLoad.items [Some { Query.kind = Some `ExactPackage;
                                       path = Some lid;
                                       type_ = None }]
    with
    | `EmptyQuery | `Error | `Funny -> notfound name
    | `Ok (_, [_dist, _id, (_ps, _path, _loc, _infoopt, desc)], search_time, group_time) ->
        begin match desc with
        | Item.Package (pack, paths) ->
            package search_time group_time pack paths
        | _ -> assert false
        end
    | `Ok (_, [], _, _) -> 
        notfound name
    | `Ok (_, xs, _, _) -> 
        !!% "Ambig: %a@."
          Format.(list " " string)
          & List.map (fun (_, _, (_, path, _, _, _)) ->
            Path.name & Ppath.to_path_for_printing path) xs;
        ambiguous name
  in
  Lwt.return
  & html
    oco_head
  & body & div_class "search" [ oco_logo
                              ; oco_form None ]
           :: res

let () = 
  Eliom_registration.Html5.register
    ~service & fun arg () ->
      query arg

