open Spotlib.Spot
open Asttypes
(* open Conv *)

type 'typ kind = 
  | Class
  | ClassType
  | ClassField of virtual_flag * 'typ
  | Constr of 'typ
  | Exception of 'typ
  | Field of 'typ
  | Method of private_flag * virtual_flag * 'typ
  | ModType
  | Module
  | Type of 'typ list (** type params *) * 'typ option * [ `Abstract | `Record | `Variant ]
  | Value of 'typ 
  | Package of OCamlFind.Package.t * string list (** top modules, ex. "Dbm" *)

(* with conv(json) *)

type t =
    OCamlFind.Package.t list (* CR jfuruse: for Package, it is meaningless *)
    * Ppath.t
    * Location.t
    * (Odoc_info.info option, OCamlDoc.error) Result.t
    * Xtype.t kind

val rec_hcons : t -> t
val format : Format.formatter -> t -> unit
