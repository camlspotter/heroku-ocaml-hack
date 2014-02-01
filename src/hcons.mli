(* Hash consing *)
val string    : string -> string
val ident     : Ident.t -> Ident.t
val longident : Longident.t -> Longident.t
val path_equal : Path.t -> Path.t -> bool
val path      : Path.t -> Path.t
val position : Lexing.position -> Lexing.position
val location : Location.t -> Location.t
