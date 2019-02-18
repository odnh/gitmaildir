open Core
open Cmdliner
open Lwt.Infix

module Store = Git.Store.Make(Digestif.SHA1)(Git_unix.Fs)(Git.Inflate)(Git.Deflate)
module Git_ops = Gitmaildir.Git_ops.Make(Store)
module Locking_unix = Gitmaildir_unix.Locking_unix
module Maildir = Gitmaildir.Maildir.Make_granular(Git_ops)(Locking_unix)

(* helper functions *)

let lift_error err = (err :> Git_ops.error)

let err_out err =
  Git_ops.pp_error Format.std_formatter err;
  Out_channel.newline stdout

let store_of_string path =
  Store.v () Fpath.(v path)
  >|= function
    | Ok s -> s
    | Error _ -> failwith "Bad Store"

(* extra cli only functions *)

let convert_maildir store path =
  let rec get_all_files result = function
    | f::fs when Sys.is_directory f = `Yes ->
        Sys.readdir f
        |> Array.to_list
        |> List.map ~f:(Filename.concat f)
        |> List.append fs
        |> get_all_files result
    | f::fs -> get_all_files (f::result) fs
    | [] -> result in
  let maildir_root_length = Fpath.segs path |> List.length in
  let all_files = get_all_files [] [Fpath.to_string path] in
  let sorted_files = all_files
    |> List.map ~f:(fun f -> f, (Unix.stat f).st_mtime)
    |> List.sort ~compare:(fun (_,t1) (_,t2) -> Float.compare t1 t2)
    |> List.map ~f:(fun (f,t) ->
        (Fpath.(v f |> segs)
          |> (fun l -> List.drop l maildir_root_length)
          |> String.concat ~sep:Fpath.dir_sep
        , f, t)) in
  List.iter sorted_files ~f:(fun (f1, f2, t) ->
    let input = In_channel.create f2 in
    let deliver = Maildir.add_mail_time t store (Fpath.v f1) input in
    let result = Lwt_main.run deliver in
    In_channel.close input;
    match result with
    | Ok () -> ()
    | Error e -> err_out e; failwith "Conversion Error")

(* deliver command *)

let deliver store =
  let store = store_of_string store in
  let deliver_lwt = store >>= (fun s -> Maildir.deliver_mail s In_channel.stdin) in
  match Lwt_main.run deliver_lwt with
  | Ok _ -> ()
  | Error e -> err_out e

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
  | Error e -> err_out e

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
  | Error e -> err_out e

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
  | Error e -> err_out e

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
  let store = Lwt_main.run @@ store_of_string store in
  let path = Fpath.v path in
  let init_lwt = Maildir.init_gitmaildir store in
  match Lwt_main.run init_lwt with
  | Ok () -> Lwt_main.run @@  Maildir.convert_maildir store path |> (fun _ -> ())(*convert_maildir store path*)
  | Error e -> err_out e

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

(* init command *)

let init store =
  let store = store_of_string store in
  let init_lwt = store >>= (fun s -> Maildir.init_gitmaildir s) in
  match Lwt_main.run init_lwt with
  | Ok _ -> ()
  | Error e -> err_out e

let init_store_arg =
  let doc = "Path of the gitmailidr to initialise" in
  let env = Arg.env_var "GITMAILDIR_PATH" ~doc in
  let doc = "The gitmaildir path" in
  Arg.(value & opt string "." & info ["d"; "dir"] ~env ~docv:"PATH" ~doc)

let init_info =
  let doc = "Initialise a gitmaildir" in
  Term.info "init" ~doc ~exits:Term.default_exits

let init_t = Term.(const init $ init_store_arg)

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
    convert_t, convert_info;
    init_t, init_info
  ]

let () = Term.exit @@ multi_command
