open Spotlib.Spot
open Eliom_content.Html5.D (* provides functions to create HTML nodes *)

let param = 
  let open Eliom_parameter in
   opt & string "q"

let main_service =
  Eliom_registration.Html5.register_service
    ~path:["graff"]
    ~get_params:param
    (fun q () ->
      Lwt.return
        (html
           (head (title (pcdata "Page title")) [])
           (body [ h1 [ pcdata & String.concat "\n" & snd & Unix.shell_command_stdout "uname -a"
                      ]
                 ; pcdata & "Query: " ^ (Option.default q (fun () -> "NONE"))
                 ])))
