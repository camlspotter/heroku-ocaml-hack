(** Packages + PATH *)

open Spotlib.Spot
open Path

open Ocaml_conv

type t = 
  | PPpredef
  | PPpack of OCamlFind.Packages.t (** can be linked into more than one package *) 
  | PPident of string
  | PPdot of t * string
  | PPapply of t * t
with conv(ocaml)

type t_ = t

let hcons_equal p1 p2 = match p1, p2 with
  | PPpredef, PPpredef -> true
  | PPpack ps1, PPpack ps2 -> ps1 == ps2 (* already hconsed *)
  | PPident s1, PPident s2 -> s1 == s2
  | PPdot (t1, s1), PPdot (t2,s2) -> t1 == t2 && s1 == s2 (* string hconsed *)
  | PPapply (t11, t12), PPapply (t21, t22) -> t11 == t21 && t12 == t22
  | _ -> false

module H = Hashcons.Make(struct
  type t = t_
  let equal = hcons_equal
  let hash = Hashtbl.hash
end)

let hcons = H.hcons
      
let rec of_path p = hcons & match p with
  | Pident id -> 
      let name = Ident.name id in
      if name = "*predef*" then PPpredef
      else if name.[0] = '{' then begin
        (* It is an ident made from a package name *)
        PPpack (OCamlFind.Packages.of_string & String.(sub name 1 & length name - 2))
      end else 
        PPident (Hcons.string name)
  | Pdot (t, n, _) -> PPdot (of_path t, Hcons.string n)
  | Papply (t1, t2) -> PPapply (of_path t1, of_path t2)
  
let package_path name =
  let name = "{" ^ name ^ "}" in
  let id = Ident.create_persistent name in
  Path.Pident id

let rec to_path_for_printing = function
  | PPpack pks -> package_path & OCamlFind.Packages.to_string_for_printing pks
  | PPident s -> Pident ( Ident.create_persistent s )
  | PPdot (PPpredef, s) -> Pident ( Ident.create_persistent s )
  | PPdot (t, n) -> Pdot (to_path_for_printing t, n, 0)
  | PPapply (t1, t2) -> Papply (to_path_for_printing t1,
                                to_path_for_printing t2)
  | PPpredef -> Pident ( Ident.create_persistent "*predef*" )

let rec to_path_for_debug = function
  | PPpack pks -> package_path & OCamlFind.Packages.to_string pks
  | PPident s -> Pident ( Ident.create_persistent s )
  | PPdot (t, n) -> Pdot (to_path_for_debug t, n, 0)
  | PPapply (t1, t2) -> Papply (to_path_for_debug t1,
                                to_path_for_debug t2)
  | PPpredef -> Pident ( Ident.create_persistent "*predef*" )

let to_path_for_printing p = 
  try to_path_for_printing p with
  | e ->
      !!% "Ppath.to_path_for_printing: %a@."
        Xprinttyp.path & to_path_for_debug p;
      raise e

let format ppf p = Xprinttyp.path ppf & to_path_for_printing p

open Longident

let rec of_longident = function
  | Lident s -> hcons & PPident (Hcons.string s)
  | Ldot (Lident "*predef*", s) -> 
      (* hack for option *)
      of_longident (Lident s)
  | Ldot (t, s) -> hcons & PPdot (of_longident t, Hcons.string s)
  | Lapply (t1, t2) -> hcons & PPapply (of_longident t1, of_longident t2)

let rec to_longident = function
  | PPident s -> Lident s
  | PPpredef -> Lident "*predef*"
  | PPdot (p, s) -> Ldot (to_longident p, s)
  | PPapply (p1, p2) -> Lapply (to_longident p1, to_longident p2)
  | PPpack _ -> assert false



