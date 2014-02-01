(* This should be ported / already ported to spotlib *)

module String = struct
  open String

  let find_with_pos_opt ?(from=0) f str =
    let len = length str in
    let rec loop pos =
      if pos >= len then None
      else 
        let c = unsafe_get str pos in
        if f c then Some (c, pos)
        else loop (pos+1)
    in
    loop from
  
  let split1 ?from f str =
    match find_with_pos_opt ?from f str with
    | None -> None
    | Some (_,pos) -> Some (sub str 0 pos, sub str (pos+1) (length str - pos - 1))
end


