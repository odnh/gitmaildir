open Core
open Lwt.Infix

module Store = Git_unix.Store

let checkout_to_dir store commit path =
  Lwt.return_unit >>= fun () ->
  Lwt.return @@ Sys.command ("mkdir " ^ Fpath.to_string path) >>= fun _ ->
  Lwt.return @@ Sys.command ("git --git-dir=" ^ (Fpath.to_string (Store.dotgit store))
    ^ " --work-tree=" ^ (Fpath.to_string (Fpath.to_dir_path path))
    ^ " checkout " ^ (Store.Hash.to_hex commit) ^ " -- .") >>= fun _ ->
  Lwt.return_unit
