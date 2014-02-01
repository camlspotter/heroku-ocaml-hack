(** OPAM tools 

Note that these functions do not use OPAM API but results of opam cli commands,
so they may be inaccurate, and are fragile to any change of opam command output.

CR jfuruse: I should change this to use OPAM API. But for now it is not very stable.    
*)

open Spotlib.Spot

val get_prefix : unit -> string
(** Check OPAM installation and returns its prefix *)

val current_switch : string
(** The current OPAM switch *)

type package = {
  name : string;
  version : string;
  desc : string;
  base : bool; (** true means it is a virtual base package *)
}

val format_package : Format.t -> package -> unit
val name_of_package : package -> string

val get_installed : unit -> package list
(** Obtain installed OPAM packages *)

val package_dir_of : string -> (string * string) option
(** Given the path of a build file, deduce its OPAM switch and package 
    This is simply done by file path name:
    [package_of ".../.opam/system/camlidl/com.a" = Some ("system", "camlidl")]

    BUG: the path must contains ".opam" directory name.
*)

module Make(A : sig end) : sig
  
  
  val opam_build_dir : string
  (** OPAM build dir *)
  
  val installed : package list
  (** Installed OPAM packages *)
  
  val package_of : string -> package option
  
  (*
  val package_build_dir : package -> [> `Base | `Found of string ]
  
  val get_built_table : package -> [> `Base | `Found of FileDigest.tbl ]
  
  val build_tables : (package * [> `Base | `Found of FileDigest.tbl ]) list
  *)
  
  val all_build_table : FileDigest.tbl lazy_t
  (** Digest table of all the built files in OPAM build directory *)
  
  val get_base_package : OCamlFind.Package.t -> package option
  (** Special mapping rule from OCamlFind base packages to OPAM package names *)
  
  val guess_package :
    OCamlFind.Package.t  (** The package *)
    -> (string * string lazy_t) list  (** Installed file digests *)
    -> [> `Ambiguous
       | `Base of package
       | `Found of package
       | `Maybe of package * int
       | `NotFound ]
  (** Guess OPAM package from OCamlFind package *)
  
  val guess_build_dir : base:string -> digest:Digest.t -> string list
  (** General file build dir guessing *)
  
end
  
  
