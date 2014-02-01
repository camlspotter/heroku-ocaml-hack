(** OCamlFind tools *)

module Package : sig

(*
  type raw = Fl_package_base.package = {
    package_name : string;
    package_dir : string;
    package_defs : Fl_metascanner.pkg_definition list;
    package_priv : Fl_package_base.package_priv;
  }
  (** Same as [Fl_package_base.package]. *)
*)

  type t = { 
    name : string;
    dir : string;
    defs : (string * string) list
  }
  (** Simplified version of [Fl_package_base.package] *)

  val name : t -> string
  (** Get package_name. It is shorter to type. *)
  
  val find_var : string -> t -> string option
  (** Find a variable binding *)

  val version : t -> string option
  (** The version of the package *)

  val requires : t -> string list option
  (** Required packages. [None] means the "requires" field itself is not found *)

  val top_name : t -> string
  (** returns the top packgae name: i.e. "camlimages" for "camlimages.core" *)

  val parse_browse_interfaces : t -> string list option
  (** Wierd hack for "distributed with Ocaml" things 
      
      META for the packages from OCaml distribution contains
      a strange field browse_interfaces. It is a very strange string
      but helps to know which modules belong to which base package.
  *)

  val has_browse_interfaces : t -> bool
  (** [true] if the package has "browse_interface" field *)

  val is_top : t -> bool
  (** [true] if the package is a top one *)

  val group : t list -> (string, t list) Hashtbl.t
  (** Group packages by their top package names *)
end

type ocamlfind
(** Witness of initialization *)

val init : unit -> ocamlfind
(** Initialization of FindLib *)

val get_packages : ocamlfind -> Package.t list
(** Get the installed packages *)

val get_stdlib_dir : ocamlfind -> string
(** Get the stdlib directory name *)

val installed_cmi_resolver : 
  Package.t 
  -> modname: string 
  -> digest:Cmfile.CMIDigest.t 
  -> Module_path.t option
(** Scan the installation directory of the package and list up all the cmi files 
    with their signature digests.

    This hack is required since some modules are linked into a cma but their cmi's
    are installed in a sub directory and not in the directory specified by META.
    ex. CamlP4.
 *)


type package_modules = {
  targets : (Module_path.t * Cmfile.CMIDigest.t) list;
  (** The linked modules *)

  reachable_tops : (Module_path.t * string list (* ml path *) * Cmfile.CMIDigest.t (** cmi md5 *) option) list 
  (** Compilation units linked in. If the library is packed, this contains
      lot more modules than [targets]. *)
}

val get_package_modules : 
  FileDigest.tbl
  -> stdlib_dir:string 
  -> Package.t 
  -> package_modules

val find_packages 
  : (Package.t * package_modules) list 
  -> file_path: string
  -> (Package.t * string list) list option
(** Given file path of the module, deduce the packages which provide it.
    The corresponding cmi file is required.
*)
(* CR jfuruse: this should return Ppath.t list option. (But currently it introduce circular deps) *)

val choose_best_package_name : string list -> string
(** Choose the shortest and canonical name of the given package names.
    
    [ choose_best_package_name [ "camlimages"; "camlimages.core" ] 
      = "camlimages" ]

    [ choose_best_package_name [ "camlimages.core"; "camlimages.exif" ] 
      = "camlimages.core" ] (* alphabetical order is chosen *)
 *)

module Packages : sig
    type t = Package.t list with conv(ocaml)

    val to_string : t -> string
    val to_string_for_printing : t -> string
    val of_string : string -> t

    val exact_string_of : t -> string
(*
    val of_exact_string : string -> t
*)
end
