open Core
open Cmdliner

module Store = Git.Store.Make(Digestif.SHA1)(Git_unix.Fs)(Git.Inflate)(Git.Deflate)
module Git_ops = Gitmaildir.Git_ops.Make(Store)
module Locking_unix = Gitmaildir_unix.Locking_unix
module Maildir = Gitmaildir.Maildir.Make_granular(Git_ops)(Locking_unix)

let run_daemon store path =
  let store = Lwt_main.run @@ Store.v () Fpath.(v store) in
  let path = Fpath.v path in
  let _ = Result.map store ~f:(fun s -> Main.run_daemon s path) in
  ()

let store_arg =
  let doc = "Path of the git store to sync" in
  let env = Arg.env_var "GITMAILDIR_PATH" ~doc in
  let doc = "The git store path" in
  Arg.(value & opt string "." & info ["g"; "gitstore"] ~env ~docv:"PATH" ~doc)
  
let path_arg =
  let doc = "Path of the maildir to sync" in
  let env = Arg.env_var "GITMAILDIR_PATH" ~doc in
  let doc = "The maildir path" in
  Arg.(value & opt string "." & info ["m"; "maildir"] ~env ~docv:"PATH" ~doc)

let daemon_info =
  let doc = "Keep a maildir in sync with its git representation" in
  let man = [
    `S Manpage.s_bugs;
    `P "To report bugs, open an issue at github.com/odnh/gitmaildir" ]
  in
  Term.info "gitmaildir_daemon" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

let daemon_t =
    Term.(const run_daemon $ store_arg $ path_arg)

let () = Term.exit @@ Term.eval (daemon_t, daemon_info)
