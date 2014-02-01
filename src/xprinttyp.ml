(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2012 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

open Spotlib.Spot

  open Printtyp

  let make_type ppf f ?with_pos:(_=false) ty =
(*
    let ty = if with_pos then TypeFix.type_expr ty else ty in
*)
    f ppf ty

  let type_expr ?with_pos ppf = make_type ppf type_expr ?with_pos
  let type_sch ?with_pos ppf = make_type ppf type_sch ?with_pos
  let type_scheme ?with_pos ppf = make_type ppf type_scheme ?with_pos
  let modtype ?with_pos:(_=false) ppf mty = 
(*
    let mty = if with_pos then TypeFix.module_type mty else mty in
*)
    modtype ppf mty


open Format
open Path

let ident_pervasive = Ident.create_persistent "Pervasives"

let ident ppf id = Format.string ppf & Xpath.escape_operator & Ident.name id

let rec path ppf = function
 | Pident id ->
     ident ppf id
 | Pdot(Pident id, s, _pos) when Ident.same id ident_pervasive ->
     Format.string ppf & Xpath.escape_operator & s
 | Pdot(p, s, _pos) ->
     fprintf ppf "%a.%s" path p & Xpath.escape_operator s
 | Papply(p1, p2) ->
     fprintf ppf "%a(%a)" path p1 path p2
