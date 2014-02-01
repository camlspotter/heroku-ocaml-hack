(* Eliom search result pager *)

open Spotlib.Spot
open List.Infix
open Eliom_content.Html5.D

let items_per_page = 20
let pager_nav_max_pages = 10

let pager_nav ~total pos =
  assert (total > 0);
  assert (0 < pos && pos <= total);
  let first_page = max 1 & pos - pager_nav_max_pages / 2 in
  let last_page = min total & first_page + pager_nav_max_pages - 1 in
  (if first_page > 1 then [`First] else [])
  @ (if pos > 2 then [`Prev] else [])
  @ List.map (fun i -> if i = pos then `Here else `Goto i) (first_page--last_page)
  @ (if pos = total then [] else [`Next])

let pager ~item ~next ~prev ~here ~goto items =
  assert (items <> []);
  let pages = List.splits_by items_per_page items in
  let num_pages = List.length pages in
  fun n ->
    let group = List.nth pages (n-1) in
    let pager_nav = pager_nav ~total:num_pages n in
    let trs = List.map item group in
    [ table ~a:[ a_class [ "pager" ] ] (List.hd trs) (List.tl trs)
        (* CR jfuruse: this is the approach for non-empty list, but looks slightly stupid.
           any good tool around?
        *)
    ; ul ~a:[ a_class [ "jump" ] ]
        ( List.map (function
            | `First -> li [ goto 1 ]
            | `Here -> li [ here n ]
            | `Next -> li [ next (n+1) ]
            | `Prev -> li [ prev (n-1) ]
            | `Goto i -> li [ goto i ]) pager_nav )
    ]

      
      
    
    
