open Core
open Git_ops

type t

(** -------------------- Helper functions -------------------- *) 

let lwt_option_bind_both a b = Lwt.bind b (function | Some s -> a s | None -> Lwt.return_none)

let lwt_option_bind2 f a b =
  Lwt.bind a (function
    | Some a -> Lwt.bind b (function
      | Some b -> f a b
      | None -> Lwt.return_none)
    | None -> Lwt.return_none)

let (>>==) a b = lwt_option_bind_both b a

let get_new_email_filename () =
  (string_of_int (int_of_float ((Unix.gettimeofday ()) *. 1_000_000.)))
  ^ "."
  ^ (string_of_int (Random.bits ())) (*TODO: make randomness more inline with official spec*)
  ^ "."
  ^ (Unix.gethostname ())

(** -------------------- Main functions -------------------- *) 

let deliver_mail store input =
  let mail_name = Git.Path.v @@ get_new_email_filename () in
  let master_ref = Git.Reference.master in
  let master_commit = get_master_commit store in
  let master_tree = master_commit >>== get_commit_tree store in
  add_blob_to_store store input
  |> lwt_option_bind2 (fun a b -> add_hash_to_tree store a mail_name b) master_tree
  |> lwt_option_bind2 (fun a b -> commit_tree store a "deliver mail" b) master_commit
  >>== update_ref store master_ref

(*
let move_mail store name new_name =
  match hash_from_filename store name with
  | None -> failwith "Email does not exist"
  | Some hash ->
      let tree = get_head_tree store in
let new_tree = remove_from_tree tree hash in
add_to_tree new_tree new_name hash |> mktree store
  |> commit_tree store |> update_head store

let delete_mail store name =
  match hash_from_filename store name with
  | None -> failwith "Email does not exist"
  | Some hash ->
      let tree = get_head_tree store in
remove_from_tree tree hash |> mktree store
  |> commit_tree store |> update_head store
*)
