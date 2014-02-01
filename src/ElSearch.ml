open Spotlib.Spot
open Eliom_content.Html5.D (* provides functions to create HTML nodes *)
open ElMisc

let service = ElServices.Search.service

let hlink_index es =
  a ~service es None

let hlink_query ?pos string es =
  a ~service es
    (Some (string, pos)) (* q=query+p=3 *)

let example string = hlink_query string [ !$ string ]

let examples = 
  [ h2 [ !$ "Examples" ]
  ; ul ~a: [ a_class [ "examples" ] ]
    [ li [ example "'a list -> 'b list" ]
    ; li [ example "string list -> string" ]
    ; li [ example "concat" ]
    ; li [ example "(+)" ]
    ; li [ example "float : _" ]
(*    ; li [ example "* : float" ] *)
(*
    ; li [ example "{stdlib}._" ]
    ; li [ example "{stdlib}.*" ]
*)
    ; li [ example "constr Ok" ]
    ; li [ example "(_) : _" ]
    ]
  ]

let packages = a ~service:ElServices.Packages.service [ !$ "Packages" ]

let index = Lwt.return 
  & html oco_head
  & body & [ oco_logo
           ; oco_form None
           ]
         @ examples
         @ [ p [ ElServices.Packages.a [ !$ "Packages" ] ()
               ; !$ " "; issues ] ]

let queries qs = !$ ("Queries: " ^ (String.concat ", " & List.map Query.string_of qs))

let found qs q search_time group_time items page =
  let len = List.length items in
  p_class "status" [ !$ !% "%d results (%0.4f secs) (%0.4f secs for search, %0.4f secs for grouping)" len (search_time +. group_time) search_time group_time]
  :: p [ queries qs ]
  :: ElPager.pager 
         ~item:ElItem.item
         ~here:(fun n -> div_class "here" [ !$ (string_of_int n) ])
         ~next:(fun n -> hlink_query ~pos:n q [ div_class "next" [ !$ "Next" ] ])
         ~prev:(fun n -> hlink_query ~pos:n q [ div_class "prev" [ !$ "Prev" ] ])
         ~goto:(fun n -> hlink_query ~pos:n q [ div_class "goto" [ !$ (string_of_int n) ] ])
         items
         page

let notfound qs = 
  [ p_class "status" [ !$ "No match" ]
  ; p [ queries qs ]
  ]

let funny = 
  [ p_class "status" [ !$ "You made a Funny query" ]
  ]

let error = 
  [ p_class "status" [ !$ "Invalid query" ] 
  (* ; examples  *)
  ]

let query q nopt = 
  match Query.search ElLoad.items q with
  | `Empty -> index
  | _ ->
      Lwt.return 
      & html 
        oco_head
      & body & div_class "search" [ oco_logo
                                  ; oco_form (Some q) ]
             :: (match Query.search ElLoad.items q with
                 | `EmptyQuery -> assert false
                 | `Ok (qs,[],_,_) -> notfound qs
                 | `Error -> error
                 | `Funny -> funny
                 | `Ok (qs, items, search_time, result_time) -> 
                     found qs q search_time result_time items (Option.default nopt & fun () -> 1))
  
let () = 
  Eliom_registration.Html5.register
    ~service & fun argopt () ->
      match argopt with
      | None -> index
      | Some (q,nopt) -> query q nopt (* q *)
