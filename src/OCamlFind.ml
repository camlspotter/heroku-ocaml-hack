open Spotlib.Spot

open Orakuda
module Scanner = Fl_metascanner

module Package = struct

  type raw = Fl_package_base.package = 
      { package_name : string;
        package_dir : string;
        package_defs : Scanner.pkg_definition list;
        package_priv : Fl_package_base.package_priv
      }

  type t = { 
    name : string;
    dir : string;
    defs : (string * string) list
  }

  let convert t = 
    { name = t.package_name;
      dir = t.package_dir;
      defs = List.map (fun def -> def.Scanner.def_var, def.Scanner.def_value) t.package_defs }

  let name p = p.name

  let find_var v p = List.assoc_opt v p.defs

  let version = find_var "version"

  let requires p = 
    match find_var "requires" p with
    | None -> None
    | Some s -> Some (Regexp.split <:m<[\s,]+>> s) 

  let top_name p = 
    match XSpotlib.String.split1 (function '.' -> true | _ -> false) p.name with
    | Some (x,_) -> x
    | None -> p.name

  (* Wierd hack for "distributed with Ocaml" things 
      
     META for the packages from OCaml distribution contains
     a strange field browse_interfaces. It is a very strnage string
     but helps to know which modules belong to which base package.
  *)
  let parse_browse_interfaces p = 
    find_var "browse_interfaces" p
    |> Option.map (fun v ->
      List.filter_map (fun s ->
        let s = <:s<\s//g>> s in
        match s with
        | "" -> None
        | _ -> Some s) 
            & Regexp.split <:m< Unit name: >> v)
      
  let has_browse_interfaces p = 
    List.exists (fun (k,_) -> k = "browse_interfaces") p.defs
  
  let is_top p = not & String.contains p.name '.'

  let group ps =
    let tbl = Hashtbl.create 107 in
    List.iter (fun p -> Hashtbl.alter tbl (top_name p) (function
      | None -> Some [p]
      | Some ps -> Some (p::ps))) ps;
    tbl
end

type ocamlfind = unit

let init () = Findlib.init ()

let get_packages () =
  List.map (Fl_package_base.query *> Package.convert) & Fl_package_base.list_packages ()

let get_stdlib_dir () = Findlib.ocaml_stdlib ()

type package_modules = {
  targets : (Module_path.t * Cmfile.CMIDigest.t) list;
  reachable_tops : (Module_path.t * string list (** module name *) * Cmfile.CMIDigest.t option) list
}

let scan_installed_files =
  (* almost of all the subpackages use the same dir of its parents. So memoizing is required. *)
  let f dir = 
    let res = ref [] in
    Unix.Find.find [dir] ~f:(fun path ->
      match Filename.split_extension path#base with
      | _body, "" -> ()
      | _body, dot_ext ->
          res := (path#path, Module_path.of_path path#path, dot_ext) :: !res);
    !res
  in
  let f = memoize f in
  fun p -> f & p.Package.dir


let installed_cmi_resolver p = 
  let tbl = 
    scan_installed_files p
    |> List.filter_map (fun (_path, mpath, ext) ->
      match ext with
      | ".cmi" -> 
          begin match Cmfile.cmi_md5 mpath with
          | Some (_, digest) -> Some ((Module_path.modname mpath, digest), mpath)
          | None -> !!% "???: %s@." & Module_path.to_string mpath; assert false
          end
      | _ -> None)
    |> Hashtbl.of_list 107
  in
  fun ~modname ~digest ->  Hashtbl.find_opt tbl (modname, digest)
  
(* META directory thing

directory = "^"   for stdlib dir

./compiler-libs/META:directory= "+compiler-libs"
./threads/META:  directory = "+vmthreads"
./threads/META:  directory = "+threads"
./ocamlbuild/META:directory= "^ocamlbuild"
./eliom/META:directory = "server"
./eliom/META:directory = "client"
./eliom/META:directory = "syntax"

*)

(* This does not really find mli only module but cmis w/o values.
   But even if such a module has ml and cmo, it can be accessed
   from other packages as an mli only module, so it is ok.
*)
let find_mli_only_module = memoize & fun dir ->
  let found = ref [] in
  let open List.Infix in
  (* !!% "Finding mli only module in %s...@." dir; *)
  Unix.Find.(find [dir] ~f:(fun path ->
    if path#is_dir && path#depth > 0 then prune ()
    else if String.is_postfix ".cmi" path#base then
      if Cmfile.cmi_without_value path#path then 
        (* CR jfuruse: double read of cmi files *)
        let mpath = Module_path.of_path path#path in
        (* CR jfuruse: it is redundant. *)
        let cmi_md5 = match Cmfile.cmi_md5 mpath with 
          | None -> assert false
          | Some (_, d) -> d
        in
        found +::= (mpath, cmi_md5)
      else ()
    else ()));
  !found
  |- 
    List.iter (fun (mpath, _) ->
      !!% "mli only module: %s@." & Module_path.to_string mpath)

(* CR jfuruse: BUG: mli only modules are ignored by this method.
*)
let get_package_modules build_table ~stdlib_dir p =
  Util.with_marshaled_cache ("data/ocamlfind_" ^ p.Package.name ^ ".bin") & fun () ->
  !!% "Scanning OCamlFind package %s...@." p.Package.name;
  let defs = p.Package.defs in
  let dir = p.Package.dir in

  let targets, mpath_md5s = 
    (* stdlib_dir is "contaminated", so we need a special handling *)
    if p.dir = stdlib_dir then begin
      match Package.parse_browse_interfaces p with
      | None -> assert false
      | Some modules -> 
          !!% "%s is with browse interface list@." & p.Package.name;
          let mpath_md5s = 
            List.filter_map (fun n -> 
              let mpath = Module_path.of_string & stdlib_dir ^/ n in
              match Cmfile.cmi_md5 mpath with
              | None -> 
                  (* lib/ocaml/arith_flags.cmi is not installed,
                     while there is arith_flags.cmx!
                  *)
(* CR jfuruse:
Warning: No cmi file for /home/jun/.share/prefix/lib/ocaml/Condition
Warning: No cmi file for /home/jun/.share/prefix/lib/ocaml/Event
Warning: No cmi file for /home/jun/.share/prefix/lib/ocaml/Mutex
Warning: No cmi file for /home/jun/.share/prefix/lib/ocaml/Thread
Warning: No cmi file for /home/jun/.share/prefix/lib/ocaml/ThreadUnix
*)
                  !!% "Warning: No cmi file for %s@." & Module_path.to_string mpath;
                  None
              | Some (_path, d) -> Some (mpath, d))
              modules
          in
          mpath_md5s, 
          List.map (fun (mpath,digest) -> 
            mpath, [ Module_path.modname mpath ], Some digest) mpath_md5s
    end else begin
      assert (p.Package.name <> "stdlib");
  
      let files_in_archive = 
        List.(
          filter_map (fun (k,v) ->
            if k = "archive" then Some v else None) defs
          |> concat_map (String.split (function ' ' -> true | _ -> false))
          |> map (fun s -> dir ^/ s)
        )
      in
  
      let resolver = installed_cmi_resolver p in
      Cmfile.load_archive build_table resolver files_in_archive
    end
  in
  let mli_only_modules = 
    (* Special handling of stdlib dir *)
    if dir = stdlib_dir && p.Package.name <> "stdlib" then []
    else find_mli_only_module dir 
  in
  let targets = List.unique & targets @ mli_only_modules in
  let reachable_tops = List.unique (mpath_md5s @ List.map (fun (m,d) -> m, [ Module_path.modname m], Some d) mli_only_modules) in
  { targets; reachable_tops } 
  |- fun _ ->
    if Conf.show_scanned_ocamlfind_module_list then begin
      List.iter (fun (mpath, md5) ->
        !!% "  %s %s@." (Cmfile.CMIDigest.to_string md5) (Module_path.to_string mpath)) targets;
      List.iter (fun (mpath, ml_path, md5) ->
        !!% "  TOPS: %s %s %s@." 
          (match md5 with Some md5 -> Cmfile.CMIDigest.to_string md5 | None -> "--------------------------------") 
          (String.concat "." ml_path)
          (Module_path.to_string mpath)) reachable_tops
    end
(*
  |- fun _ ->
    !!% "    targets=%d tops=%d@." 
      (List.length targets) (List.length mpath_md5s)
*)

(*
  |- fun { reachable_tops } ->
    !!% "@[<2>SCAN %s:@ @[<v>%a@]@]@."
      p.Package.Package.name
      Format.(list "@," (fun ppf (mpath, dopt) ->
        fprintf ppf "%s: %s"
          (match dopt with 
          | None -> "NO MD5" 
          | Some d -> Digest.to_hex d)
          (Module_path.to_string mpath))) reachable_tops
*)


let make_cmi_md5_packages_tbl xs =
  let tbl = Hashtbl.create 1023 in
  ~~ List.iter xs ~f:(fun (p, { reachable_tops= mpath_md5_list }) ->
    ~~ List.iter mpath_md5_list ~f:(fun (mpath, ml_path, md5) ->
      Hashtbl.alter tbl (Module_path.modname mpath, md5) (function
      | None -> Some [p, ml_path]
      | Some ps -> Some ((p, ml_path)::ps))));
  tbl

let find_packages package_modules =
  let cmi_md5_packages_tbl = make_cmi_md5_packages_tbl package_modules in
  fun path ->
    try
      let mpath = Module_path.of_path path in
      match Cmfile.cmi_md5 mpath with
      | None -> 
          !!% "Warning: No cmi for %s@." path;
          None 
      | Some (_p, md5) -> 
          let modname = Module_path.modname mpath in
          begin try
                  match Hashtbl.find cmi_md5_packages_tbl (modname, Some md5) with
                  | [] ->
                      !!% "Warning: %s:%s in cmi_md5_packages_tbl but = []@." path (Cmfile.CMIDigest.to_string md5);
                      assert false
                  | v -> Some v
            with
            | Not_found ->
                (* the same cmi was not found in OCamlFind installation directory *)
                !!% "Warning: No OCamlFind package (so inaccessible) for %s %s@." (Cmfile.CMIDigest.to_string md5) path;
                None
          end
    with
    | _ -> assert false

let find_packages package_modules_list = 
  let f = memoize & find_packages package_modules_list in
  fun ~file_path -> f file_path 
(*
  |- !!% "find_packages: %s : %a@."
      path
      (Format.option (Format.list ",@ " (fun ppf p -> Format.string ppf p.Package.name)))
*)

let choose_best_package_name = function 
  | [] -> assert false
  | pack_names ->
      let open List in
      pack_names
      |> map (fun s -> String.length s, s)
      |> sort_then_group (fun (l1, _) (l2, _) -> compare l1 l2) 
      |> hd
      |> map snd
      |> sort compare 
      |> hd 

module Packages = struct

  type t = Package.t list

  (** A module can be linked with more than one packages *)

  let name_packages_tbl = Hashtbl.create 101

  let cntr = UniqueID.create ()

  let exact_string_of ps = 
    "{" 
    ^ String.concat "," (List.map (fun p -> p.Package.name) ps)
    ^ "}"

(*
  let of_exact_string s =
    let open Option in
    String.is_prefix' "{" s >>= fun s ->
    String.is_postfix' "}" s >>= fun s -> 
    Some (String.split (function ',' -> true | _ -> false) s)
*)

  (* Making a name from [t] must be reversible: We must be able to get the orignal [t]
     from the name. *)
  let to_string = memoize & fun t ->
    let name = 
      match t with
      | [] -> "<no packages>"
      | _ -> 
          let s = choose_best_package_name (List.map (fun p -> p.Package.name) t) in
          let id = UniqueID.get cntr in
          !% "%s#%d" s id
    in
    name |- fun name -> Hashtbl.replace name_packages_tbl name t
  
  let to_string_for_printing t =
    match String.split (function '#' -> true | _ -> false) & to_string t with
    | [name; _] -> name
    | ["<no packages>"] -> "<no packages>"
    | _ -> assert false
  
  let of_string s = try Hashtbl.find name_packages_tbl s with Not_found -> 
    !!% "No packages found for %s@." s;
    raise Not_found

  let ocaml_of_t t = Ocaml.String (to_string t)
  let t_of_ocaml_exn ?trace:_ = function
    | Ocaml.String s -> of_string s
    | _ -> assert false
  let t_of_ocaml = Ocaml_conv.result t_of_ocaml_exn
end
