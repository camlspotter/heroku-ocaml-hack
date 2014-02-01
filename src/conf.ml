open Spotlib.Spot

module M = struct
  let dump = ref false
  let rev_files = ref []
  let compiler_source_dir = ref ""
  let show_ocamldoc_message = ref false
  let show_cache_loading = ref false
  let show_scanned_ocamlfind_module_list = ref false

  let () = 
    let f = fun s -> rev_files +::= s in
    Arg.(parse [ "-d", Set dump, "dump" 
               ; "-c", Set_string compiler_source_dir, "compiler source dir" 
               ; "--show-ocamldoc-message", Set show_ocamldoc_message, "show ocamldoc log"
               ; "--show-cache-loading", Set show_cache_loading, "show cache log"
               ; "--show-scanned-ocamlfind-module-list", Set show_scanned_ocamlfind_module_list, "show scanned OCamlFind modules"
               ] f "oco")
end

let dump = !M.dump
let files = List.rev !M.rev_files
let compiler_source_dir = !M.compiler_source_dir
let show_ocamldoc_message = !M.show_ocamldoc_message
let show_cache_loading = !M.show_cache_loading
let show_scanned_ocamlfind_module_list = !M.show_scanned_ocamlfind_module_list

let () = assert (not dump || compiler_source_dir <> "")


