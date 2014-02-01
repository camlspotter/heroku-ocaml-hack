open Spotlib.Spot

val add_pack_name : 
  (string (** module name *)
   -> (OCamlFind.Package.t * string list (** ml path *)) list option (** the module *))
  -> (string * int, Path.t) Hashtbl.t (** Local ident cache *)
  -> Path.t (** the root *) 
  -> Path.t 
  -> Ppath.t

module FromResult : sig

  val convert' : 
    (Path.t -> Ppath.t) 
    -> Types.type_expr Item.kind
    -> Xtype.t Item.kind

  (* CR jfuruse: apparently this function does not touch all the tuple elements. *)
  val convert :
    (Path.t -> Ppath.t) 
    -> (Ppath.t * Location.t * (Odoc_info.info option, OCamlDoc.error) Result.t 
        * Types.type_expr Item.kind) list
    -> (Ppath.t * Location.t * (Odoc_info.info option , OCamlDoc.error) Result.t
        * Xtype.t Item.kind) list
end
