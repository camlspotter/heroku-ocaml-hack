val reset_envs : unit -> unit

val structure : 
  Path.t 
  -> Typedtree.structure 
  -> (Path.t * Location.t * Types.type_expr Item.kind) list 
     * (string * int, Path.t) Hashtbl.t

val signature : 
  Path.t 
  -> Typedtree.signature 
  -> (Path.t * Location.t * Types.type_expr Item.kind) list 
     * (string * int, Path.t) Hashtbl.t
