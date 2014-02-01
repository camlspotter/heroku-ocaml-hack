open Spotlib.Spot
open OCamlFind.Package

type t = OCamlFind.Package.t list

type t_ = t

module H = Hashcons.Make(struct
  type t = t_
  let equal = (=) (* CR jfuruse: can be better *)
  let hash = Hashtbl.hash
end)

let normalize = H.hcons ** List.sort (fun p1 p2 -> compare p1.name p2.name)

let of_packages = normalize
let to_packages = id
