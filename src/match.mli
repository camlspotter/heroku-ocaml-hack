val match_path_type :
  Longident.t * Parsetree.core_type 
  -> Ppath.t * Xtype.t 
  -> int -> int 
  -> int option
val match_type : ?no_target_type_instantiate:bool -> Parsetree.core_type -> Xtype.t -> int -> int option
val match_path : Longident.t -> Ppath.t -> int -> int option
