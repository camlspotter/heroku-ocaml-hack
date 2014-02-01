(* This should be ported / already ported to spotlib *)

module String : sig
  val find_with_pos_opt : ?from:int ->
    (char -> bool) -> string -> (char * int) option

  val split1 : ?from:int ->
    (char -> bool) -> string -> (string * string) option
end
