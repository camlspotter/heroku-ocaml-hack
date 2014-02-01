type kind = [ `Class 
            | `ClassField
            | `ClassType
            | `Constr
            | `Exception
            | `Field
            | `Method
            | `ModType
            | `Module
            | `Type
            | `Value 
            | `Package 
            | `ExactPackage ]

type t = { 
  kind : kind option;
  path : Longident.t option;
  type_ : Parsetree.core_type option;
}

module Parse : sig
  val path : string -> t option
  val type_ : string -> t option
  val path_type : string -> t option
  val parse_kind : string -> [> kind ] option
  val prefixed : string -> t option
  val prefixed_and_type : string -> t option
  val query : string -> t option list
end

val kind_to_string : [< kind ] -> string

val string_of : t -> string

val query :
  Item.t array
  -> t option list 
  -> [> `EmptyQuery
     | `Error
     | `Funny
     | `Ok of t list * (int * int * Item.t) list * float * float 
     ]

val search :
  Item.t array
  -> string 
  -> [> `EmptyQuery
     | `Error
     | `Funny
     | `Ok of t list * (int * int * Item.t) list * float * float 
     ]

val cui : Item.t array -> 'a

val cli : unit -> unit
