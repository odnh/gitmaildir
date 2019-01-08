open Core
open Cmdliner
open Lwt_result_helpers

let deliver store =
  let store = Git_ops.store_of_string store in
  let deliver_lwt = store >>== (fun s -> Maildir.deliver_mail s In_channel.stdin) in
  match Lwt_main.run deliver_lwt with
  | Ok _ -> ()
  | Error _ -> failwith "ERROR"

let deliver_store =
  let doc = "Path of the gitmaildir to deliver to." in
  let env = Arg.env_var "GITMAILDIR_PATH" ~doc in
  let doc = "The gitmaildir path" in
  Arg.(value & pos 0 string "." & info [] ~env ~docv:"PATH" ~doc)

let deliver_t = Term.(const deliver $ deliver_store)

let info =
  let doc = "Manage a gitmaildir (git extension to the maildir format)" in
  let man = [
    `S Manpage.s_bugs;
    `P "To report bugs, open an issue at github.com/odnh/gitmaildir." ]
  in
  Term.info "gitmaildir" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (deliver_t, info)
