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

