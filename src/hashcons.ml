type 'a hcons = 'a
external (!>) : 'a hcons -> 'a = "%identity"

module Make(A : Hashtbl.HashedType) = struct
  module Elem = A
  include Weak.Make(A)
  let tbl = create 1023

  let hcons v = 
    try find tbl v with Not_found ->
      add tbl v;
      v

  (* We do not need to clear since it is weak
    let clear () = clear tbl
  *)
end

(* 
   type 'a hcons = 'a

   type t = 
     | A of s hcons
     | B of string hcons
     | C of z hcons
   and s =
     | D of t list hcons

   with hcons(X)

=>

   type t = 
     | A of s hcons
     | B of string hcons
     | C of z hcons
   and s =
     | D of t hcons list
   
   module X = struct
     type t = 
       | A of s_hc
       | B of string_hc
       | C of z_hc
     and s =
       | D of t_hc list
     and t_hc = t_hc' Hashcons.t
     and s_hc = s_hc' Hashcons.t
   end

   type t_hc = X.t_hc
   type s_hc = X.s_hc

   val t_hc : X.t -> t_hc
   val s_hc : X.s -> s_hc

   val t_hc_rec : t -> t_hc
   val s_hc_rec : s -> t_hc

*)
