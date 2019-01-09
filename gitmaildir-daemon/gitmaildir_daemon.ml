open Core
open Cmdliner

let run_daemon store = (fun a -> ())

let store_arg =
  let doc = "Path of the gitmaildir to manage" in
  let env = Arg.env_var "GITMAILDIR_PATH" ~doc in
  let doc = "The gitmaildir path" in
  Arg.(value & opt string "." & info ["d"; "dir"] ~env ~docv:"PATH" ~doc)

let daemon_info =
  let doc = "Keep a maildir in sync with it's git representation" in
  let man = [
    `S Manpage.s_bugs;
    `P "To report bugs, open an issue at github.com/odnh/gitmaildir" ]
  in
  Term.info "gitmaildir_daemon" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

let daemon_t =
    Term.(const run_daemon $ store_arg)
  
let () = Term.exit @@ Term.eval (daemon_t, daemon_info)
