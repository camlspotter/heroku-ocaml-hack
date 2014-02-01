open Spotlib.Spot
open Longident

let name p = 
  let open Buffer in
  let b = create 100 in
  let add_id s = add_string b & Xpath.escape_operator s in
  let rec name = function
    | Lident id -> add_id id
    | Ldot(p, s) -> name p; add_char b '.'; add_id s
    | Lapply(p1, p2) -> name p1; add_char b '('; name p2; add_char b ')'
  in
  name p;
  Buffer.contents b


