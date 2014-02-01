open Spotlib.Spot
open Odoc_types

(* CR jfuruse: [kind] should be moved to more general place *) 
type kind = Type | Value | Class | Class_type | Exception | Module | Module_type with conv(ocaml)

type entry = string * location * info * kind

val format_entry : Format.t -> entry -> unit
val format_info : Format.t -> Odoc_info.info -> unit
val info_to_string : Odoc_types.info -> string (* CR jfuruse: very rough *) 

type error = string list * [ `Chdir 
                           | `Exec of Unix.process_status * string list
                           | `Load_dump of exn ]

val docs_of_cmt : Cmt_format.cmt_infos -> (entry list, error) Result.t
