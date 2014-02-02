(* Display info of an OPAM package *)

open Spotlib.Spot
open Eliom_content.Html5.D (* provides functions to create HTML nodes *)
open ElMisc

module OCP = OCamlFind.Package

let service = ElServices.Package.service

let notfound name = 
  [ p_class "status" [ !$% "%S: no such OPAM package in DB" name ]
  ]

let ambiguous name = 
  [ p_class "status" [ !$% "%S: the query is ambiguous" name ]
  ]

let package search_time group_time pack mpaths =
  let ocamlfinds = 
    ~~ List.filter_map ElLoad.ocamlfind_opam_table ~f:(fun (p, opam) ->
      Option.bind opam (fun name -> 
        prerr_endline name;
        if name = pack then Some p
        else None))
  in
  let version = "xxx" in
  [ p_class "status" [ !$% "(%0.4f secs) (%0.4f secs for search, %0.4f secs for grouping)" (search_time +. group_time) search_time group_time]

  ; 
    let trs = 
       [ tr [ td_class "package_left" [ !$ "OPAM Package:" ]
            ; td [ span_class "package" [ !$ pack ]
                 ; !$% " (%s)" version
                 ]
            ]

       ; tr [ td [ !$ "Provided OCamlFind packages:" ]
            ; td [ !$  (String.concat " " 
                        & List.map (fun oc -> OCamlFind.Package.name oc) ocamlfinds) 
                 ]
            ]
         
(*
       ; tr [ td [ !$ "Description:" ]
            ; td [ !$ (find_var "description") ]
            ]

       ; tr [ td [ !$ "Requires:" ]
            ; td & match OCP.requires pack with
                   | Some [] -> [ !$ "none" ]
                   | None -> [ !$ "(no info)" ]
                   | Some packs -> 
                       List.intersperse (!$ " ")
                       & List.map (fun pack ->
                         ElServices.Package.a [ !$ pack ] pack) packs
            ]

       ; tr [ td [ !$ "Modules:" ]
            ; td & List.intersperse (!$ " ")
                     (List.map (fun p -> !$ p) mpaths)
            ]
*)
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
  prerr_endline "Registering /package service...";
  Eliom_registration.Html5.register
    ~service & fun arg () ->
      query arg

