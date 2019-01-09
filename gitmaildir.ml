open Core
open Cmdliner
open Lwt_result_helpers

let deliver store =
  let store = Git_ops.store_of_string store in
  let deliver_lwt = store >>== (fun s -> Maildir.deliver_mail s In_channel.stdin) in
  match Lwt_main.run deliver_lwt with
  | Ok _ -> ()
  | Error _ -> failwith "ERROR"

let store_arg =
  let doc = "Path of the gitmaildir to deliver to." in
  let env = Arg.env_var "GITMAILDIR_PATH" ~doc in
  let doc = "The gitmaildir path" in
  Arg.(required & pos 0 (some string) None & info [] ~env ~docv:"PATH" ~doc)

let deliver_t = Term.(const deliver $ store_arg)

let move store path new_path =
  let store = Git_ops.store_of_string store in
  let path = Fpath.v path in
  let new_path = Fpath.v new_path in
  let move_lwt = store >>== (fun s -> Maildir.move_mail s path new_path) in
  match Lwt_main.run move_lwt with
  | Ok _ -> ()
  | Error _ -> failwith "ERROR"
  
let path_arg =
  let doc = "current path of file to move" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"MALI_PATH" ~doc)

let new_path_arg =
  let doc = "path to move file to" in
  Arg.(required & pos 2 (some string) None & info [] ~docv:"NEW_MAIL_PATH" ~doc)

let move_t = Term.(const move $ store_arg $ path_arg $ new_path_arg)

let info =
  let doc = "Manage a gitmaildir (git extension to the maildir format)" in
  let man = [
    `S Manpage.s_bugs;
    `P "To report bugs, open an issue at github.com/odnh/gitmaildir" ]
  in
  Term.info "gitmaildir" ~version:"0.1" ~doc ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (deliver_t, info)
