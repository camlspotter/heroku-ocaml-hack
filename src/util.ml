open Spotlib.Spot

let with_marshaled_cache path loader = 
  if File.Test._f path then begin
    (* CR jfuruse: ocamldoc style sanity check *)
    if Conf.show_cache_loading then !!% "Loading cache, %s ...@." path;
    with_ic (open_in path) input_value
  end else begin
    loader () |- fun res -> with_oc (open_out_bin path) (fun oc -> output_value oc res)
  end

module Filename = struct
  include Filename
    
  let change_extension name ~ext =
    Filename.chop_extension name ^ "." ^ ext
end
