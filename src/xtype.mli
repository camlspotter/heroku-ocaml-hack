open Spotlib.Spot

type t =
  | Link of [ `Linked of t (** Loop (and intermidiate state) *) 
            | `Stub (** Placeholder only for intermidiate state *) 
            ] ref
      (** Outside of Xtype, Link has always [{contents= `Linked t }]
          and it indicates a loop in a type: the linked [t] is always
          a ancestor of the node. *)
  | Nil
  | Any
  | VarNamed of int * string
  | UnivarNamed of int * string
  | Var of int
  | Univar of int
  | Arrow of string * t * t
  | Tuple of t list
  | Constr of datatype * t list
  | Object of ((string * t) list * [ `Closed | `Open of t]) option * (Ppath.t * t list) option
  | Alias of t * string
  | Variant of xrow
  | Poly of t * t list
  | Package of Ppath.t * (Ppath.t * t) list

and xrow = {
  xrow_fields : (string * [ `Either of bool * t list | `Present of t option | `Absent ]) list;
  xrow_more   : t option;
  xrow_closed : bool;
  xrow_fixed  : bool;
  xrow_name   : (Ppath.t * t list) option;
}

and datatype = Ppath.t (** name *) 
               * (t list * t) option ref (** alias *)

val is_recursive : t -> bool
(*
val hcons : t -> t
val hcons_datatype : datatype -> datatype
*)

val subst : t list (** vars *) -> t list (** substs *) -> t -> t

exception Error

val of_core_type : Parsetree.core_type -> t

val of_type_expr : (Path.t -> Ppath.t) -> Types.type_expr -> t
val to_type_expr : (Ppath.t -> Path.t) -> t -> Types.type_expr

val of_type_exprs : (Path.t -> Ppath.t) -> Types.type_expr list -> t list
val to_type_exprs : (Ppath.t -> Path.t) -> t list -> Types.type_expr list

val predefined_types : string list
(** Name of predefined types such as int, float, etc. *)

val pathfix_for_printing : Path.t option -> Path.t -> Path.t
val format : Path.t option -> Format.t -> t -> unit

val format_core_type : Format.t -> Parsetree.core_type -> unit
(** Printer of [Parsetree.core_type]. It is done by translation from [core_type] to [Xtype.t] to [Types.type_expr]. Very inefficient. *)

val rec_hcons : t -> t
val rec_hcons_datatype : datatype -> datatype
