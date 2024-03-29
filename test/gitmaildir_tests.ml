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

(* Pretty prints error and fails the test *)
let print_err e =
  Git_ops.pp_error Format.std_formatter e; Alcotest.fail "See printed error message"

(* Runs a git command and outputs returns its output *)
let run_git_command cmd =
  let ic = Unix.open_process_in @@ "git --git-dir="^tmp_dir^"/.git "^cmd in
  let output = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  output

(* Runs a git command and outputs returns its output *)
let run_two_git_command cmd1 cmd2 =
  let ic = Unix.open_process_in @@ "git --git-dir="^tmp_dir^"/.git "^cmd1^" git --git-dir="^tmp_dir^"/.git "^cmd2 in
  let output = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  output

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

let get_master_commit () =
  run_lwt_check_err ~f:(fun _ -> ()) @@ Git_ops.get_master_commit store

let get_commit_tree () =
  let hash = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.get_master_commit store in
  run_lwt_check_err ~f:(fun _ -> ()) @@ Git_ops.get_commit_tree store hash

let get_commit_parents () =
  let hash = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.get_master_commit store in
  run_lwt_check_err ~f:(fun _ -> ()) @@ Git_ops.get_commit_parents store hash

let get_branch_commit () =
  run_lwt_check_err ~f:(fun _ -> ()) @@ Git_ops.get_branch_commit store Git.Reference.(of_string "refs/heads/master")

let add_blob_to_tree () =
  let ic = In_channel.create sample_email in
  let blob_hash = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.add_blob_to_store store ic in
  let commit = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.get_master_commit store in
  let tree_hash = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.get_commit_tree store commit in
  In_channel.close ic;
  run_lwt_check_err ~f:(fun _ -> ()) @@ Git_ops.add_blob_to_tree store tree_hash Git.Path.(v "mail1") blob_hash 

let add_blob_to_tree_extend () =
  let ic = In_channel.create sample_email in
  let blob_hash = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.add_blob_to_store store ic in
  let commit = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.get_master_commit store in
  let tree_hash = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.get_commit_tree store commit in
  In_channel.close ic;
  run_lwt_check_err ~f:(fun _ -> ()) @@ Git_ops.add_blob_to_tree_extend store tree_hash Git.Path.(v "new/mail1") blob_hash 

let remove_entry_from_tree () =
  let commit = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.get_master_commit store in
  let tree_hash = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.get_commit_tree store commit in
  run_lwt_check_err ~f:(fun _ -> ()) @@ Git_ops.remove_entry_from_tree store tree_hash Git.Path.(v "mail1")

let modify_tree () =
  let commit = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.get_master_commit store in
  let tree_hash = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.get_commit_tree store commit in
  run_lwt_check_err ~f:(fun _ -> ()) @@ Git_ops.modify_tree ~f:(fun a -> a) store tree_hash Git.Path.(v "") 

let commit_tree () =
  let ic = In_channel.create sample_email in
  let blob_hash = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.add_blob_to_store store ic in
  let commit = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.get_master_commit store in
  let tree_hash = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.get_commit_tree store commit in
  In_channel.close ic;
  let new_tree = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.add_blob_to_tree_extend store tree_hash Git.Path.(v "new/mail1") blob_hash in
  run_lwt_check_err ~f:(fun _ -> ()) @@ Git_ops.commit_tree store [commit] "Test commit" new_tree

let make_ref () =
  let path = "refs/heads/master" in
  let gpath = Git.Path.(v path) in
  Alcotest.(check string) "Ref same as source" (Git_ops.make_ref gpath |> Git.Reference.to_string) path

let hash_of_ref () =
  run_lwt_check_err ~f:(fun _ -> ()) @@ Git_ops.hash_of_ref store Git.Reference.master

let update_ref () =
  let commit = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.get_master_commit store in
  let ref = Git_ops.make_ref Git.Path.(v "refs/heads/master") in
  let ic = In_channel.create sample_email in
  let blob_hash = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.add_blob_to_store store ic in
  In_channel.close ic;
  let tree_hash = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.get_commit_tree store commit in
  let new_tree = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.add_blob_to_tree_extend store tree_hash Git.Path.(v "new/mail1") blob_hash in
  let new_commit = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.commit_tree store [commit] "Test commit" new_tree in
  run_lwt_check_err ~f:(fun _ -> ()) @@ Git_ops.update_ref store ref new_commit

let get_hash_at_path () =
  let commit = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.get_master_commit store in
  let tree_hash = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.get_commit_tree store commit in
  run_lwt_check_err ~f:(fun _ -> ()) @@ Git_ops.get_hash_at_path store tree_hash Git.Path.(v "new/mail1")

let read_blob () =
  let commit = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.get_master_commit store in
  let tree_hash = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.get_commit_tree store commit in
  let blob_hash = run_lwt_check_err ~f:(fun a -> a) @@ Git_ops.get_hash_at_path store tree_hash Git.Path.(v "new/mail1") in
  run_lwt_check_err ~f:(fun _ -> ()) @@ Git_ops.read_blob store blob_hash

let git_ops_set = [
  "init_empty_blob", `Quick, init_empty_blob;
  "add_blob_to_store", `Quick, add_blob_to_store;
  "get_master_commit", `Quick, get_master_commit;
  "get_commit_tree", `Quick, get_commit_tree;
  "get_commit_parents", `Quick, get_commit_parents;
  "get_branch_commit", `Quick, get_branch_commit;
  "add_blob_to_tree", `Quick, add_blob_to_tree;
  "add_blob_to_tree_extend", `Quick, add_blob_to_tree_extend;
  "remove_entry_from_tree", `Quick, remove_entry_from_tree;
  "commit_tree", `Quick, commit_tree;
  "make_ref", `Quick, make_ref;
  "hash_of_ref", `Quick, hash_of_ref;
  "update_ref", `Quick, update_ref;
  "get_hash_at_path", `Quick, get_hash_at_path;
  "read_blob", `Quick, read_blob;
  "modify_tree", `Quick, modify_tree;
]

(* maildir tests *)

let init_gitmaildir () =
  let result = Lwt_main.run @@ Maildir.init_gitmaildir store in
  let git_output = run_git_command "ls-tree HEAD" in
  match result with
  | Ok () -> Alcotest.(check string) ".keep created in new store" "100644 blob e69de29bb2d1d6434b8b29ae775ad8c2e48c5391\t.keep\n" git_output
  | Error e -> print_err e

let deliver_mail () =
  let result = Lwt_main.run @@ Maildir.deliver_mail store (In_channel.create sample_email) in
  let git_output = run_git_command "ls-tree HEAD | grep new | wc -l" in
  match result with
  | Ok () -> Alcotest.(check string) "blob created in new dir" "       1\n" git_output
  | Error e -> print_err e

let add_mail_time () =
  let result = Lwt_main.run @@ Maildir.add_mail_time (Unix.time ()) store Fpath.(v "new/mail1") (In_channel.create sample_email) in
  let git_output = run_two_git_command "ls-tree HEAD | grep new | awk '{print $3}' | xargs" "ls-tree | grep mail1 | wc -l" in
  match result with
  | Ok () -> Alcotest.(check string) "mail1 delivered to new dir" "       1\n" git_output
  | Error e -> print_err e

let add_mail () =
  let result = Lwt_main.run @@ Maildir.add_mail store Fpath.(v "new/mail2") (In_channel.create sample_email) in
  let git_output = run_two_git_command "ls-tree HEAD | grep new | awk '{print $3}' | xargs" "ls-tree | grep mail2 | wc -l" in
  match result with
  | Ok () -> Alcotest.(check string) "blob created in new dir" "       1\n" git_output
  | Error e -> print_err e

let move_mail () =
  let result = Lwt_main.run @@ Maildir.move_mail store Fpath.(v "new/mail2") Fpath.(v "new/mail3") in
  let git_output = run_two_git_command "ls-tree HEAD | grep new | awk '{print $3}' | xargs" "ls-tree | grep mail3 | wc -l" in
  match result with
  | Ok () -> Alcotest.(check string) "blob created in new dir" "       1\n" git_output
  | Error e -> print_err e

let delete_mail () =
  let result = Lwt_main.run @@ Maildir.delete_mail store Fpath.(v "new/mail3") in
  let git_output = run_two_git_command "ls-tree HEAD | grep new | awk '{print $3}' | xargs" "ls-tree | grep mail3 | wc -l" in
  match result with
  | Ok () -> Alcotest.(check string) "blob created in new dir" "       0\n" git_output
  | Error e -> print_err e

let maildir_set = [
  "init_gitmaildir", `Quick, init_gitmaildir;
  "deliver_mail", `Quick, deliver_mail;
  "add_mail_time", `Quick, add_mail_time;
  "add_mail", `Quick, add_mail;
  "move_mail", `Quick, move_mail;
  "delete_mail", `Quick, delete_mail;
]

(* run *)

let () =
  Alcotest.run "Gitmaildir tests" [
    "git_ops", git_ops_set;
    "maildir", maildir_set;
]

(* remove the testing directory and sample email *)
let () = Sys.command_exn ("rm -rf " ^ tmp_dir)
let () = Sys.command_exn ("rm -rf " ^ sample_email)
