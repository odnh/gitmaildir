open Core
open Git_ops

type t

let get_new_email_filename () =
  (string_of_int (int_of_float ((Unix.gettimeofday ()) *. 1_000_000.)))
  ^ "."
  ^ (string_of_int (Random.bits ())) (*TODO: make randomness more inline with official spec*)
  ^ "."
  ^ (Unix.gethostname ())

let deliver_mail store input =
  hash_object store input
  |> add_to_tree (get_head_tree store) (get_new_email_filename ())
  |> mktree store
  |> commit_tree store
  |> update_head store

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
