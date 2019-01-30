open Core
open Cmdliner
open Lwt.Infix

module Store = Git.Store.Make(Digestif.SHA1)(Git_unix.Fs)(Git.Inflate)(Git.Deflate)
module Git_ops = Gitmaildir.Git_ops.Make(Store)
module Maildir = Gitmaildir.Maildir.Make(Git_ops)(Locking_unix)

let lift_error err = (err :> Git_ops.error)

let store_of_string path =
  Store.v () Fpath.(v path)
  >|= function
    | Ok s -> s
    | Error _ -> failwith "Bad Store"

(* deliver command *)

let deliver store =
  let store = store_of_string store in
  let deliver_lwt = store >>= (fun s -> Maildir.deliver_mail s In_channel.stdin) in
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
  let store = store_of_string store in
  let path = Fpath.v path in
  let new_path = Fpath.v new_path in
  let move_lwt = store >>= (fun s -> Maildir.move_mail s path new_path) in
  match Lwt_main.run move_lwt with
  | Ok _ -> ()
  | Error _ -> failwith "ERROR"

let move_path_arg =
  let doc = "current path of file to move" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"SRC" ~doc)

let move_new_path_arg =
  let doc = "path to move file to" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"DEST" ~doc)

let move_info =
  let doc = "Move a file in the gitmaildir" in
  Term.info "move" ~doc ~exits:Term.default_exits

let move_t = Term.(const move $ store_arg $ move_path_arg $ move_new_path_arg)

(* delete command *)

let delete store path =
  let store = store_of_string store in
  let path = Fpath.v path in
  let delete_lwt = store >>= (fun s -> Maildir.delete_mail s path) in
  match Lwt_main.run delete_lwt with
  | Ok _ -> ()
  | Error _ -> failwith "ERROR"

let delete_path_arg =
  let doc = "path of file to delete" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH" ~doc)

let delete_info =
  let doc = "Delete a file in the gitmaildir" in
  Term.info "delete" ~doc ~exits:Term.default_exits

let delete_t = Term.(const delete $ store_arg $ delete_path_arg)

(* add command *)

let add store path =
  let store = store_of_string store in
  let path = Fpath.v path in
  let input = In_channel.stdin in
  let add_lwt = store >>= (fun s -> Maildir.add_mail s path input) in
  match Lwt_main.run add_lwt with
  | Ok _ -> ()
  | Error _ -> failwith "ERROR"

let add_path_arg =
  let doc = "path of where to add mail" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH" ~doc)

let add_info =
  let doc = "Add a mail to an arbitrary location in the gitmaildir" in
  Term.info "add" ~doc ~exits:Term.default_exits

let add_t = Term.(const add $ store_arg $ add_path_arg)

(* convert command *)

let convert store path =
  print_endline @@ "STORE: " ^ store;
  print_endline @@ "PATH: " ^ path;
  let store = store_of_string store in
  let path = Fpath.v path in
  let init_lwt = store >>= (fun s -> Maildir.init_gitmaildir s) in
  (match Lwt_main.run init_lwt with
  | Ok _ -> ()
  | Error _ -> failwith "ERROR1");
  let convert_lwt = store >>= (fun s -> Maildir.convert_maildir s path) in
  match Lwt_main.run convert_lwt with
  | Ok _ -> ()
  | Error _ -> failwith "ERROR2"

let convert_store_arg =
  let doc = "Path of directory to become new gitmaildir" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"GIT_PATH" ~doc)

let convert_path_arg =
  let doc = "Path of maildir to convert" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"MAILDIR_PATH" ~doc)

let convert_info =
  let doc = "Convert a maildir to a gitmaildir" in
  Term.info "convert" ~doc ~exits:Term.default_exits

let convert_t = Term.(const convert $ convert_store_arg $ convert_path_arg)

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
  [ deliver_t, deliver_info;
    move_t, move_info;
    delete_t, delete_info;
    add_t, add_info;
    convert_t, convert_info
  ]

let () = Term.exit @@ multi_command
