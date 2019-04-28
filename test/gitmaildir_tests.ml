open Core

(* prepare a usable module for testing (Unix filesystem that is) *)
module Store = Git.Store.Make(Digestif.SHA1)(Git_unix.Fs)(Git.Inflate)(Git.Deflate)
module Git_ops = Gitmaildir.Git_ops.Make(Store)
module Locking_unix = Gitmaildir_unix.Locking_unix
module Maildir = Gitmaildir.Maildir.Make_granular(Git_ops)(Locking_unix)

(* create dir to run tests *)
let tmp_dir = Filename.temp_dir "gitmaildir_tests" "store_directory"
let store = match Lwt_main.run Store.(v () Fpath.(v tmp_dir)) with
            | Ok s -> s
            | Error e -> (Store.pp_error Format.std_formatter e;
                         Out_channel.newline stdout;
                         failwith "Could not create store for testing")

(* Does the extra work for testing Lwt_result returning functions *)
let run_lwt_check_err lwt ~f =
  match Lwt_main.run lwt with
  | Ok a -> f a
  | Error e -> Git_ops.pp_error Format.std_formatter e; Alcotest.fail "See printed error message"

(* Create test data *)

let sample_email = Filename.temp_file "gitmaildir_tests" "sample_email"
let () = let oc = Out_channel.create sample_email in
         Out_channel.output_string oc "sample email";
         Out_channel.close oc

(* git_ops tests *)

let init_empty_blob () =
  run_lwt_check_err ~f:(Alcotest.(check unit) "unit" ()) @@ Git_ops.init_empty_blob store

let add_blob_to_store () =
  let ic = In_channel.create sample_email in
  run_lwt_check_err ~f:(fun _ -> ()) @@ Git_ops.add_blob_to_store store ic;
  In_channel.close ic

let commit_tree () = ()

let modify_tree () = ()

let add_blob_to_tree () = ()

let add_blob_to_tree_extend () = ()

let remove_entry_from_tree () = ()

let make_ref () = ()

let hash_of_ref () = ()

let update_ref () = ()

let get_master_commit () = ()

let get_branch_commit () = ()

let get_commit_parents () = ()

let get_commit_tree () = ()

let get_hash_at_path () = ()

let read_blob () = ()


let git_ops_set = [
  "init_empty_blob", `Quick, init_empty_blob;
  "add_blob_to_store", `Quick, add_blob_to_store;
  "commit_tree", `Quick, commit_tree;
  "modify_tree", `Quick, modify_tree;
  "add_blob_to_tree", `Quick, add_blob_to_tree;
  "add_blob_to_tree_extend", `Quick, add_blob_to_tree_extend;
  "remove_entry_from_tree", `Quick, remove_entry_from_tree;
  "make_ref", `Quick, make_ref;
  "hash_of_ref", `Quick, hash_of_ref;
  "update_ref", `Quick, update_ref;
  "get_master_commit", `Quick, get_master_commit;
  "get_branch_commit", `Quick, get_branch_commit;
  "get_commit_parents", `Quick, get_commit_parents;
  "get_commit_tree", `Quick, get_commit_tree;
  "get_hash_at_path", `Quick, get_hash_at_path;
  "read_blob", `Quick, read_blob;
]

(* maildir tests *)

let init_gitmaildir () =
  run_lwt_check_err ~f:(Alcotest.(check unit) "unit" ()) @@ Maildir.init_gitmaildir store

let deliver_mail () =
  run_lwt_check_err ~f:(Alcotest.(check unit) "deliver with unit" ()) @@ Maildir.deliver_mail store (In_channel.create sample_email)

let move_mail () = ()

let delete_mail () = ()

let add_mail_time () = ()

let add_mail () = ()

let maildir_set = [
  "init_gitmaildir", `Quick, init_gitmaildir;
  "deliver_mail", `Quick, deliver_mail;
  "move_mail", `Quick, move_mail;
  "delete_mail", `Quick, delete_mail;
  "add_mail_time", `Quick, add_mail_time;
  "add_mail", `Quick, add_mail;
]

(* run *)

let () =
  Alcotest.run "Gitmaildir tests" [
    "git_ops", git_ops_set;
    "maildir", maildir_set;
]
