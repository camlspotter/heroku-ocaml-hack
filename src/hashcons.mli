type 'a hcons
external (!>) : 'a hcons -> 'a = "%identity"

module Make(A : Hashtbl.HashedType) : sig
  module Elem : Hashtbl.HashedType with type t = A.t
  val hcons : A.t -> A.t
  (* val clear : unit -> unit 
     No need of clear since it is weak
  *)
end
