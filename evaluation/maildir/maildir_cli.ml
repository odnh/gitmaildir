open Core
open Cmdliner

(* deliver command *)

let store_arg =
  let doc = "Path of the maildir to deliver to." in
  let env = Arg.env_var "MAILDIR_PATH" ~doc in
  let doc = "The maildir path" in
  Arg.(value & opt string "." & info ["d"; "dir"] ~env ~docv:"PATH" ~doc)

let deliver_info =
  let doc = "Deliver an email to the maildir" in
  Term.info "deliver" ~doc ~exits:Term.default_exits

let deliver_t = Term.(const Maildir.deliver $ store_arg)

(* move command *)

let move_path_arg =
  let doc = "current path of file to move" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"SRC" ~doc)

let move_new_path_arg =
  let doc = "path to move file to" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"DEST" ~doc)

let move_info =
  let doc = "Move a file in the maildir" in
  Term.info "move" ~doc ~exits:Term.default_exits

let move_t = Term.(const Maildir.move $ store_arg $ move_path_arg $ move_new_path_arg)

(* delete command *)

let delete_path_arg =
  let doc = "path of file to delete" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH" ~doc)

let delete_info =
  let doc = "Delete a file in the maildir" in
  Term.info "delete" ~doc ~exits:Term.default_exits

let delete_t = Term.(const Maildir.delete $ store_arg $ delete_path_arg)

(* maildir command *)

let maildir_info =
  let doc = "Perform maildir test operations" in
  let man = [
    `S Manpage.s_bugs;
    `P "To report bugs, open an issue at github.com/odnh/gitmaildir" ]
  in
  Term.info "maildir_cli" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

let maildir_t =
  Term.(const ())

let multi_command = Term.eval_choice (maildir_t, maildir_info)
  [ deliver_t, deliver_info;
    move_t, move_info;
    delete_t, delete_info;
  ]

let () = Term.exit @@ multi_command
