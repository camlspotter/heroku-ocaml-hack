(* dirty hashconsing 

   I know I should use Hashcons by Jean-Christophe Filliatre 
   or something equivalent, but it enforces changes of types,
   which seems to be hard as far as I use OCaml's type_expr.

*)

open Spotlib.Spot

module String = struct
  include Hashcons.Make(struct
    type t = string
    let hash = Hashtbl.hash
    let equal (s : string) s' = s = s'
  end)
end

let string = String.hcons

module Idents = Hashcons.Make(struct
  type t = Ident.t
  open Ident
  let equal id1 id2 = 
    (* id1.stamp = id2.stamp  *)
    (* && *) id1.name == id2.name (* already hconsed *)
    (* && id1.flags = id2.flags *)
  let hash = Hashtbl.hash
end)

let ident id = Idents.hcons { id with Ident.name = string id.Ident.name } 

module Longidents = Hashcons.Make(struct
  type t = Longident.t
  open Longident
  let equal p1 p2 = match p1, p2 with
    | Lident s1, Lident s2 -> s1 == s2 (* already hconsed *)
    | Ldot (t1, s1), Ldot (t2, s2) ->
        t1 == t2 && s1 == s2 
    | Lapply (t11, t12), Lapply (t21, t22) ->
        t11 == t21 && t12 == t22
    | _ -> false
  let hash = Hashtbl.hash
end)

let rec longident l =
  let l = match l with
    | Longident.Lident id -> Longident.Lident (string id)
    | Longident.Ldot (p, s)  -> Longident.Ldot (longident p, string s)
    | Longident.Lapply (p1, p2) -> Longident.Lapply (longident p1, longident p2)
  in
  Longidents.hcons l

let path_equal p1 p2 = 
  let open Path in
  p1 == p2 
  ||
  match p1, p2 with
  | Pident id1, Pident id2 -> id1 == id2 (* already hconsed *)
  | Pdot (t1, s1, _n1), Pdot (t2, s2, _n2) ->
      t1 == t2 && s1 == s2
  | Papply (t11, t12), Papply (t21, t22) ->
      t11 == t21 && t12 == t22
  | _ -> false

module Paths = Hashcons.Make(struct
  type t = Path.t
  let equal = path_equal
  let hash = Hashtbl.hash
end)

let rec path p =
  Paths.hcons 
  & match p with
    | Path.Pident id       -> Path.Pident (ident id)
    | Path.Pdot (p, s, _n)  -> Path.Pdot (path p, string s, 0)
    | Path.Papply (p1, p2) -> Path.Papply (path p1, path p2)

(* just partial *)
let position p = 
  let open Lexing in
  { p with pos_fname = string p.pos_fname }    

(* just partial *)
let location t = 
  let open Location in
  { t with loc_start = position t.loc_start;
           loc_end = position t.loc_end }
