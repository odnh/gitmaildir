open Core
open Git_ops
open Lwt_result_helpers

type t

(* -------------------- Helper functions -------------------- *)

let get_new_email_filename () =
  (string_of_int (int_of_float ((Unix.gettimeofday ()) *. 1_000_000.)))
  ^ "."
  ^ (string_of_int (Random.bits ())) (*TODO: make randomness more inline with official spec*)
  ^ "."
  ^ (Unix.gethostname ())

(* -------------------- Main functions -------------------- *)

let deliver_mail store input =
  let mail_name = Git.Path.v @@ get_new_email_filename () in
  let master_ref = Git.Reference.master in
  let master_commit = get_master_commit store in
  let master_tree = master_commit >>== get_commit_tree store in
  add_blob_to_store store input
  |> lwt_result_bind2 (fun a b -> add_hash_to_tree store a mail_name b) master_tree
  |> lwt_result_bind2 (fun a b -> commit_tree store a "\ndeliver mail\n" b) master_commit
  >>== update_ref store master_ref

let delete_mail store path =
  let gpath = Git.Path.v (Fpath.to_string path) in
  let master_ref = Git.Reference.master in
  let master_commit = get_master_commit store in
  let master_tree = master_commit >>== get_commit_tree store in
  master_tree >>== (fun a -> remove_entry_from_tree store a gpath)
  |> lwt_result_bind2 (fun a b -> commit_tree store a "\nremove mail\n" b) master_commit
  >>== update_ref store master_ref

let move_mail store path new_path =
  let path = Git.Path.v (Fpath.to_string path) in
  let new_path = Git.Path.v (Fpath.to_string new_path) in
  let master_ref = Git.Reference.master in
  let master_commit = get_master_commit store in
  let master_tree = master_commit >>== get_commit_tree store in
  let hash = master_tree >>== (fun t -> get_hash_at_path store t path) in
  master_tree >>== (fun a -> remove_entry_from_tree store a path)
  |> lwt_result_bind2 (fun a b -> add_hash_to_tree store b new_path a) hash
  |> lwt_result_bind2 (fun a b -> commit_tree store a "\nmove mail\n" b) master_commit
  >>== update_ref store master_ref
