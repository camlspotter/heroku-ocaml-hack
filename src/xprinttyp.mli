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
open Format

val type_expr :
  ?with_pos:bool -> formatter -> Types.type_expr -> unit
val type_sch :
  ?with_pos:bool -> formatter -> Types.type_expr -> unit
val type_scheme :
  ?with_pos:bool -> formatter -> Types.type_expr -> unit
val modtype :
  ?with_pos:bool -> formatter -> Types.module_type -> unit

val ident : Format.t -> Ident.t -> unit
val path : Format.t -> Path.t -> unit
