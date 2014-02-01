open Spotlib.Spot

module Ty = Types
module P = Printtyp
open Cmt_format
open Item

module OCP = OCamlFind.Package

let get_doc docs path loc desc = 
  let open OCamlDoc in
  (* CR jfuruse: package_path is already inside path.
     It complicates the process here *)
  let path_name =
    (* CR jfuruse: does it work for binops especially ( * ) ? *)
    Xpath.name & Xpath.remove_package_path path 
  in

  (* !!% "OCAMLDOC PATHNAME=%s@." path_name; *)
  (* CR jfuruse: inefficient! *)
  let rec find = function
    | [] -> None
    | (path_name', { Odoc_types.loc_impl; loc_inter }, info, kind) :: xs ->
        let kind = match desc, kind with
          | Item.Class       , OCamlDoc.Class
          | Item.Exception _ , Exception
          | Item.ModType     , Module_type
          | Item.Module      , Module
          | Item.Type _      , Type
          | Item.Value _     , Value -> true
          | _ -> false
        in
        if
          kind
          && (
            (* Locations rarely agree *)
            (Some loc = loc_impl || Some loc = loc_inter)
            || path_name = path_name'
          )
        then Some info
        else find xs
  in
  match docs with
  | `Ok docs -> `Ok (find docs)
  | `Error e -> `Error e

let add_ocamldoc cmt items =
  let docs = OCamlDoc.docs_of_cmt cmt in
  List.map (fun (path, loc, desc) ->
    path, loc, get_doc docs path loc desc, desc) items

let rec load root find_packages path =
  Extract.reset_envs ();

  let cmt = match Cmt_format.read path with
    | _, None -> assert false
    | _, Some cmt -> cmt
  in

  (* Workaround of include path
     This is not perfect at all. But giving the dir of cmti 
     to the loading path seems to be fair.

     CR jfuruse:
     [cmt.cmt_loadpath] contains the build directory, but in relative.
     We need to make them absolute.
  *)
  let cmt_loadpath = List.map (fun s -> cmt.cmt_builddir ^/ s) cmt.cmt_loadpath in
  Config.load_path := Filename.dirname path :: cmt_loadpath;

  let root = Hcons.path & Path.Pdot (root, cmt.cmt_modname, 0) in

  (* CR jfuruse: If Result.t is found in {spotlib},  currently we make {spotlib}.Result.t
     but actually it should be {spotlib}.Spot.Result.t. The linked modules must have higher priority.
  *)
  let pathconv =
    let f modname = 
      match Cmfile.find_cmi_file modname with
      | None -> 
          !!% "%a: No cmi file found for %s@." Printtyp.path root modname;
          None
      | Some s ->
          match find_packages ~file_path:s with
          | (Some _ as res) -> res
          | None -> 
              !!% "%a: no OCamlFind package found for %s@." Printtyp.path root s;
              None
    in 
    let f = memoize f in
    fun idtable p -> 
      Pathfix.add_pack_name 
        f
        idtable
        root
        p
  in

  match cmt.cmt_annots with
  | Implementation str -> 
      let items, idtable = Extract.structure root str in
      (root, Location.none (* This should be the file itself *), Module) :: items
      |> add_ocamldoc cmt 
      |> List.map (fun (p, l, i, d) -> (Ppath.of_path p, l, i, d))
      |> Pathfix.FromResult.convert (pathconv idtable)

 |  Interface sg -> 
      let items, idtable = Extract.signature root sg in  
      (root, Location.none (* This should be the file itself *), Module) :: items
      |> add_ocamldoc cmt 
      |> List.map (fun (p, l, o, d) -> (Ppath.of_path p, l, o, d))
      |> Pathfix.FromResult.convert (pathconv idtable)

  | Packed (sg, paths) ->
      (* sg and paths must be coupled! *)

      (* We try to flush out unused signature info by GC *)
      let sg_ids = List.map (function
        | Ty.Sig_module (id, _, _) -> id
        | _ -> assert false) sg
      in

      List.combine sg_ids paths
      |> List.concat_map (fun (id, path) ->
        let path_no_ext = Filename.chop_extension path in
        let modname = String.capitalize & Filename.basename path_no_ext in
        assert (Ident.name id = modname);
        let cmti = cmt.cmt_builddir ^/ path_no_ext ^ ".cmti" in
        let cmt  = cmt.cmt_builddir ^/ path_no_ext ^ ".cmt" in
        match
          if File.Test._f cmti then Some cmti 
          else if File.Test._f cmt then Some cmt
          else None
        with
        | None ->
            !!% "Warning: %s: Submodule: either %s or %s is not found@."
              (Xpath.name root)
              cmti cmt;
            []
        | Some path ->
            load root find_packages path)

   | _ -> assert false


let load packages find_packages path =
  let root = Ppath.package_path & OCamlFind.Packages.to_string & Packageshack.of_packages packages in
  let add_packages (a,b,c,d) = (packages,Ppath.hcons a,b,c,d) in
  try 
    !!% "Loading %s (%s)...@." path (Xpath.name root);
    let items = load root find_packages path in
    !!% "Loaded %d items@." (* path *) (List.length items);
    List.map add_packages items
  with
  | (Env.Error e as exn) -> !!% "Env.Error: %a@." Env.report_error e; raise exn
  
let load packages find_packages path =

  Extract.reset_envs ();

  load packages find_packages path |- fun _ -> Extract.reset_envs ()

type dump_file = {
  top_package : OCP.t;
  packages : OCP.t list;
  opam : OPAM.package option;
  items : Item.t list
}

module Make(A : sig end) = struct
  module O = OPAM.Make(struct end)

  let guess_build_dir mpath =
    let exts = [".cmti"; ".cmt"; ".cmi"; ".cmo"; ".cmx"] in (* no source files since they can be copied *)
    let paths = List.filter_map (fun ext -> Module_path.file ext mpath) exts in
    List.concat_map (fun path ->
      let base = Filename.basename path in
      let digest = Digest.file path in
      O.guess_build_dir ~base ~digest) paths
    |> List.unique

  let load_module find_packages ps mpath =
    let open Option in
    let path = 
      Module_path.file ".cmti" mpath
      >>=! fun () -> Module_path.file ".cmt" mpath
      >>=! fun () ->
        let modname = Module_path.modname mpath in
        let build_dirs = guess_build_dir mpath in
        let rec f = function
          | [] -> None
          | d::ds ->
              let mpath = Module_path.of_string (d ^/ modname) in
              Module_path.file ".cmti" mpath 
              >>=! fun () -> Module_path.file ".cmt" mpath
              >>=! fun () -> f ds
        in
        f build_dirs
    in
    match path with
    | Some path -> load ps find_packages path
    | None -> !!% "No cmti/cmt file found for %s@." (Module_path.to_string mpath); []
  
  let dump_package_group package_modules_list mpath_packages_tbl find_packages top_package ps =
  
      let pname = OCP.name top_package in
  
      Util.with_marshaled_cache ("data/oco_" ^ pname ^ ".bin") & fun () ->
  
        (* OPAM package which provides the top package *)
        let opam =
          let mpaths = 
            List.unique
            & List.concat_map (fun (p, mods) ->
              let pn = OCP.name p in 
              if List.exists (fun p' -> OCP.name p' = pn) ps then
                (* CR jfuruse: ml_path is ignored! *)
                List.map (fun (pkg, _ml_path, _) -> pkg) mods.OCamlFind.reachable_tops
              else []) package_modules_list 
          in
          let base_digests = List.filter_map (fun mpath -> 
            let path = Module_path.file ".cmi" mpath in
            Option.bind path (fun path ->
              try
                Some (Filename.basename path,
                      lazy (Digest.file path))
              with
              | _ -> 
                  !!% "Digest failed: %s@." path;
                  None)) mpaths
          in
          !!% "@[<2>OPAM scan %s:@ @[%a@]@]@."
            (OCP.name top_package)
            Format.(list "@ " string)
            (List.map fst base_digests);
          if base_digests = [] then begin
            !!% "WARNING: OCamlFind package %s has no base_digests therefore cannot be gussed its OPAM package@." (OCP.name top_package);
          end;
          (match O.guess_package top_package base_digests with
          | `Found name -> Some name
          | `Maybe (name, _) -> Some name
          | `Base b -> Some b
          | `NotFound | `Ambiguous -> None)
            |- fun res -> 
              !!% "OCamlFind package %s is provided by OPAM package %a@."
                (OCP.name top_package)
                Format.(option (fun ppf p -> string ppf p.OPAM.name)) res
        in
  
        (* Packages are items *)
        let items_package = 
          ~~ List.filter_map package_modules_list ~f: (fun (p, { OCamlFind.targets = mpath_digest_list }) -> 
            if not & List.mem p ps then None
            else
              let path = 
                Ppath.of_path 
                & Ppath.package_path 
                & OCamlFind.Packages.to_string 
                & Packageshack.of_packages [p] 
              in
              let mpaths = 
                List.map (String.capitalize ** Filename.basename ** Module_path.to_string ** fst) mpath_digest_list
              in
  
              Some ([p], path, Location.none, `Ok None, Package (p, mpaths)))
        in
  
        (* Scan things inside modules *)
        let items_contents =
          let is = ref [] in
          ~~ Hashtbl.iter mpath_packages_tbl ~f:(fun mpath ps ->
            match List.unique & List.map OCP.top_name ps with
            | [] -> assert false
            | (_::_::_ as ns) ->
                !!% "ERROR: more than one OCamlFind top package names found: %s"
                  & String.concat " " ns;
                assert false
            | [n] ->
                if n <> pname then ()
                else is := load_module find_packages ps mpath @ !is);
          !is
        in
        
        let items_of_pname = items_package @ items_contents in
  
        { top_package; packages=  ps;  opam; items= items_of_pname }

  let prepare () =
  
    let ocamlfind = OCamlFind.init () in
    let packages = OCamlFind.get_packages ocamlfind in
  
    let stdlib_dir = OCamlFind.get_stdlib_dir ocamlfind in
  
    let package_modules_list = packages |> List.map (fun p ->
      p, OCamlFind.get_package_modules (Lazy.force O.all_build_table) ~stdlib_dir p)
    in
          
    let find_packages = OCamlFind.find_packages package_modules_list in
  
    (* Which packages a module path belongs to? *)
    (* This is used as the target list of module scanning, so reachable tops are not appropriate. *)
    let mpath_packages_tbl : (Module_path.t, OCP.t list) Hashtbl.t = 
      let tbl = Hashtbl.create 107 in
      package_modules_list |> List.iter (fun (p, { OCamlFind.targets }) ->
        targets |> List.iter (fun (mpath, _) ->
          Hashtbl.alter tbl mpath (function
            | None -> Some [p]
            | Some ps -> Some (p::ps))));
      tbl
    in
  
    let group = OCP.group packages in
    
    packages, group, package_modules_list, find_packages, mpath_packages_tbl
  
  let dump_items () =
    let _packages, group, package_modules_list, find_packages, mpath_packages_tbl =
      prepare ()
    in
    
    let (), secs = Unix.timed (fun () -> 
  
      ~~ Hashtbl.iter group ~f:(fun pname ps -> 
  
        let top_package = List.find (fun p -> OCP.name p = pname) ps in
  
        let _ : dump_file = 
          dump_package_group 
            package_modules_list 
            mpath_packages_tbl 
            find_packages 
            top_package 
            ps
        in
          
        ())) ()
    in
  
    !!% "dumped in %f secs@." secs
  
end

let dump_items () =
  let module D = Make(struct end) in
  D.dump_items ()

type db = {
  items : Item.t array;
  ocamlfind_opam_table : (OCP.t * OPAM.package option) list;
  (** List of the top OCamlFind packages
      and the OPAM package which installed it if exists.

      Note: it lists only the top packages.
  *)
}

let load_dumped_package_group path : dump_file =
  if Conf.show_cache_loading then !!% "Loading %s...@." path;
  with_ic (open_in path) input_value
    |- fun { top_package=pack; opam=opamopt; items } -> 
      if Conf.show_cache_loading then begin
        !!% "%s %a %d items@."
          (* path *)
          (OCP.name pack)
          (Format.option OPAM.format_package) opamopt 
          (List.length items)
      end

let load_dumped_items () =
  let items, ocamlfind_opam_table = 
    let items = ref [] in
    let ocamlfind_opam_table = ref [] in
    Unix.Find.find ~follow_symlink:true ["data"] ~f:(fun p ->
      p#base 
      |! <:m<^oco_.*\.bin$>> ->
          let { top_package; opam; items=items_of_pname } = load_dumped_package_group p#path in
          ocamlfind_opam_table := (top_package, opam) :: !ocamlfind_opam_table; 
          items := items_of_pname @ !items
      | _ -> ());
    !items, !ocamlfind_opam_table
  in
  !!% "%d entries loaded@." (List.length items);

  (* debug *)
  ~~ List.iter ocamlfind_opam_table ~f:(fun (p, opam_opt) ->
    !!% "%s : %a@." p.OCP.name
      (Format.option OPAM.format_package) opam_opt);

  { items = Array.of_list items;
    ocamlfind_opam_table = ocamlfind_opam_table }

let load_items () =
  let res, (stat_before, stat_after) = Gc.with_compacts load_dumped_items () in
  !!% "DB words: %.2fMb@." 
    (float (stat_after.Gc.live_words - stat_before.Gc.live_words)
     /. float (1024 * 1024 / (Sys.word_size / 8)));
  res
