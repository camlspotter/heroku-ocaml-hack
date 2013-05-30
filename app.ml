open Spotlib.Spot

let f ic oc =
  let inputs = 
    let rec loop rev_lines = 
      let s = try input_line ic with _ -> "" in 
!!% "Input %S@." s;
      if s = "\r" || s = "" then List.rev rev_lines
      else loop (s :: rev_lines)
    in
    loop []
  in
  match inputs with
  | [] -> assert false
  | r::_ ->
      let out = output_string oc in
      let out_rn s = output_string oc s; output_string oc "\r\n" in
      match String.split (function ' ' -> true | _ -> false) r with
      | "GET" :: "/opam-lib.tgz" :: _ ->
          out_rn "HTTP/1.1 200 OK";
          out_rn "Content-Type: application/octet-stream";
          out_rn "";
          let buf = String.create 10240 in
          let ic = open_in "opam-lib.tgz" in
          let rec loop () =
            let read = input ic buf 0 10240 in
            if read = 0 then close_in ic
            else begin
              output oc buf 0 read;
              loop ()
            end
          in
          loop ()
      | "GET" :: _ :: _ ->
          let _, lines = Unix.shell_command_stdout "find /vendor /tmp" in
          out_rn "HTTP/1.1 200 OK";
          out_rn "Content-Type: text/plain";
          out_rn "";
          List.iter (fun line ->
            out line;) lines;
          flush oc
      | _ -> assert false

let port =
  let port = ref None in
  Arg.parse [] (fun x -> port := Some (int_of_string x)) "app port";
  
  match !port with
  | None -> assert false
  | Some port -> port

let sockaddr = Unix.ADDR_INET (Unix.inet_addr_any, port)

let () = Unix.establish_server f sockaddr
