let version = <:qx<ocamlc -version>> |> snd |> List.hd |> Spotlib.Spot.String.chop_eols
 
