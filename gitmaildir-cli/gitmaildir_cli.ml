open Core
open Cmdliner
open Gitmaildir.Lwt_result_helpers

module Maildir = Gitmaildir.Maildir
module Git_ops = Gitmaildir.Git_ops

(* deliver command *)

let deliver store =
  let store = Gitmaildir.Git_ops.store_of_string store in
  let deliver_lwt = store >>== (fun s -> Maildir.deliver_mail s In_channel.stdin) in
  match Lwt_main.run deliver_lwt with
  | Ok _ -> ()
  | Error _ -> failwith "ERROR"

let store_arg =
  let doc = "Path of the gitmaildir to deliver to." in
  let env = Arg.env_var "GITMAILDIR_PATH" ~doc in
  let doc = "The gitmaildir path" in
  Arg.(value & opt string "." & info ["d"; "dir"] ~env ~docv:"PATH" ~doc)

let deliver_info =
  let doc = "Deliver an email to the gitmaildir" in
  Term.info "deliver" ~doc ~exits:Term.default_exits

let deliver_t = Term.(const deliver $ store_arg)

(* move command *)

let move store path new_path =
  let store = Git_ops.store_of_string store in
  let path = Fpath.v path in
  let new_path = Fpath.v new_path in
  let move_lwt = store >>== (fun s -> Maildir.move_mail s path new_path) in
  match Lwt_main.run move_lwt with
  | Ok _ -> ()
  | Error _ -> failwith "ERROR"
  
let move_path_arg =
  let doc = "current path of file to move" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"SRC" ~doc)

let move_new_path_arg =
  let doc = "path to move file to" in
  Arg.(required & pos 2 (some string) None & info [] ~docv:"DEST" ~doc)

let move_info =
  let doc = "Move a file in the gitmaildir" in
  Term.info "move" ~doc ~exits:Term.default_exits

let move_t = Term.(const move $ store_arg $ move_path_arg $ move_new_path_arg)

(* delete command *)

let delete store path =
  let store = Git_ops.store_of_string store in
  let path = Fpath.v path in
  let delete_lwt = store >>== (fun s -> Maildir.delete_mail s path) in
  match Lwt_main.run delete_lwt with
  | Ok _ -> ()
  | Error _ -> failwith "ERROR"
  
let delete_path_arg =
  let doc = "path of file to delete" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"PATH" ~doc)

let delete_info =
  let doc = "Delete a file in the gitmaildir" in
  Term.info "delete" ~doc ~exits:Term.default_exits

let delete_t = Term.(const delete $ store_arg $ delete_path_arg)

(* gitmaildir command *)

let gitmaildir_info =
  let doc = "Manage a gitmaildir" in
  let man = [
    `S Manpage.s_bugs;
    `P "To report bugs, open an issue at github.com/odnh/gitmaildir" ]
  in
  Term.info "gitmaildir_cli" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

let gitmaildir_t =
  Term.(const ())

let multi_command = Term.eval_choice (gitmaildir_t, gitmaildir_info)
  [deliver_t, deliver_info; move_t, move_info; delete_t, delete_info]

let () = Term.exit @@ multi_command