open Core
open Lwt.Infix
open Lwt_result_helpers

(* -------------------- Helper functions -------------------- *)

let get_new_email_filename () =
  (string_of_int (int_of_float ((Unix.gettimeofday ()) *. 1_000_000.)))
  ^ "."
  ^ (string_of_int (Random.bits ()))
  ^ "."
  ^ (Unix.gethostname ())

(* -------------------- Main functions -------------------- *)

module type Locking = sig
  type t

  val v : string -> t

  val lock : t -> unit

  val unlock : t -> unit
end

module type S_raw = sig

  module Store : Git.Store.S

  type error

  val deliver_mail : Store.t -> In_channel.t -> Store.Hash.t -> (Store.Hash.t, error) Lwt_result.t

  val move_mail : Store.t -> Fpath.t -> Fpath.t -> Store.Hash.t -> (Store.Hash.t, error) Lwt_result.t

  val delete_mail : Store.t -> Fpath.t -> Store.Hash.t -> (Store.Hash.t, error) Lwt_result.t

  val add_mail_time : float -> Store.t -> Fpath.t -> In_channel.t -> Store.Hash.t -> (Store.Hash.t, error) Lwt_result.t

  val add_mail : Store.t -> Fpath.t -> In_channel.t -> Store.Hash.t -> (Store.Hash.t, error) Lwt_result.t
end

module type S = sig

  module Store : Git.Store.S

  type error

  val deliver_mail : Store.t -> In_channel.t -> (unit, error) Lwt_result.t

  val move_mail : Store.t -> Fpath.t -> Fpath.t -> (unit, error) Lwt_result.t

  val delete_mail : Store.t -> Fpath.t -> (unit, error) Lwt_result.t

  val add_mail_time : float -> Store.t -> Fpath.t -> In_channel.t -> (unit, error) Lwt_result.t

  val add_mail : Store.t -> Fpath.t -> In_channel.t -> (unit, error) Lwt_result.t

  val init_gitmaildir : Store.t -> (unit, error) Lwt_result.t
end

module Make_raw (G : Git_ops.S) = struct

  module Store = G.Store
  open G

  let deliver_mail store input commit =
    let mail_name = Git.Path.v @@ ("new/" ^ get_new_email_filename ()) in
    let tree = get_commit_tree store commit in
    add_blob_to_store store input
    |> lwt_result_bind2 (fun a b -> add_blob_to_tree_extend store a mail_name b) tree
    >>== commit_tree store [commit] "deliver mail"

  let delete_mail store path commit =
    let gpath = Git.Path.v (Fpath.to_string path) in
    let tree = get_commit_tree store commit in
    tree >>== (fun a -> remove_entry_from_tree store a gpath)
    >>== commit_tree store [commit] "remove mail"

  let move_mail store path new_path commit =
    let path = Git.Path.v (Fpath.to_string path) in
    let new_path = Git.Path.v (Fpath.to_string new_path) in
    let tree = get_commit_tree store commit in
    let hash = tree >>== (fun t -> get_hash_at_path store t path) in
    tree >>== (fun a -> remove_entry_from_tree store a path)
    |> lwt_result_bind2 (fun a b -> add_blob_to_tree store b new_path a) hash
    >>== commit_tree store [commit] "move mail"

  let add_mail_time time store path input commit =
    let path = Git.Path.v (Fpath.to_string path) in
    let tree = get_commit_tree store commit in
    add_blob_to_store store input
    |> lwt_result_bind2 (fun a b -> add_blob_to_tree_extend store a path b) tree
    >>== commit_tree ~time store [commit] "deliver mail"

  let add_mail = add_mail_time (Unix.time ())
end

module Make_unsafe (G : Git_ops.S) = struct

  module Store = G.Store
  module Raw = Make_raw(G)
  open G

  let deliver_mail store input =
    let master_ref = Git.Reference.master in
    let master_commit = get_master_commit store in
    master_commit >>== Raw.deliver_mail store input
    >>== update_ref store master_ref

  let delete_mail store path =
    let master_ref = Git.Reference.master in
    let master_commit = get_master_commit store in
    master_commit >>== Raw.delete_mail store path
    >>== update_ref store master_ref

  let move_mail store path new_path =
    let master_ref = Git.Reference.master in
    let master_commit = get_master_commit store in
    master_commit >>== Raw.move_mail store path new_path
    >>== update_ref store master_ref

  let add_mail_time time store path input =
    let master_ref = Git.Reference.master in
    let master_commit = get_master_commit store in
    master_commit >>== Raw.add_mail_time time store path input
    >>== update_ref store master_ref

  let add_mail = add_mail_time (Unix.time ())

  let init_gitmaildir store =
    init_empty_blob store
end

module Make_locking (G : Git_ops.S) (L : Locking) = struct

  module Store = G.Store
  module Unsafe = Make_unsafe(G)
  module Lock = L
  open G

  let get_lock store = Lock.v ((Fpath.to_string @@ Store.root store) ^ "/.global_lock")

  let deliver_mail store input =
    let lock = get_lock store in
    Lwt.return_unit >|= (fun () ->
    Lock.lock lock) >>= (fun () ->
    Unsafe.deliver_mail store input) >|= (fun a ->
    Lock.unlock lock; a)

  let delete_mail store path =
    let lock = get_lock store in
    Lwt.return_unit >|= (fun () ->
    Lock.lock lock) >>= (fun () ->
    Unsafe.delete_mail store path) >|= (fun a ->
    Lock.unlock lock; a)

  let move_mail store path new_path =
    let lock = get_lock store in
    Lwt.return_unit >|= (fun () ->
    Lock.lock lock) >>= (fun () ->
    Unsafe.move_mail store path new_path) >|= (fun a ->
    Lock.unlock lock; a)

  let add_mail_time time store path input =
    let lock = get_lock store in
    Lwt.return_unit >|= (fun () ->
    Lock.lock lock) >>= (fun () ->
    Unsafe.add_mail_time time store path input) >|= (fun a ->
    Lock.unlock lock; a)

  let add_mail = add_mail_time (Unix.time ())

  let init_gitmaildir store =
    init_empty_blob store
end

module Make_granular (G : Git_ops.S) (L : Locking) = struct

  module Store = G.Store
  module Raw = Make_raw(G)
  module Lock = L
  open G

  let get_lock store = Lock.v ((Fpath.to_string @@ Store.root store) ^ "/.global_lock")

  (** trys to run a function on the previous master tree and commit the new one. Retries until success *)
  let rec try_with_commit f master_commit store =
    let lock = get_lock store in
    let master_ref = Git.Reference.master in
    f master_commit
    >>= (fun hash_result -> Lock.lock lock; Lwt.return hash_result) (* START LOCK *)
    >>== (fun deliver_hash ->
      get_master_commit store
      >>== (fun new_master_commit ->
        if G.Store.Hash.equal master_commit new_master_commit then
          (let res = update_ref store master_ref deliver_hash in
          Lock.unlock lock;
          res)
        else
          (Lock.unlock lock;
          try_with_commit f new_master_commit store))
    >|= (fun res -> Lock.unlock lock; res)) (* END LOCK (in case failed earlier) *)

  let deliver_mail store input =
    let mail_name = Git.Path.v @@ ("new/" ^ get_new_email_filename ()) in
    let blob_hash = add_blob_to_store store input in
    (* Have to define extra helper rather than use Raw module due to external input *)
    let deliver_blob store commit =
      let tree = get_commit_tree store commit in
      blob_hash
      |> lwt_result_bind2 (fun a b -> add_blob_to_tree_extend store a mail_name b) tree
      >>== commit_tree store [commit] "deliver mail" in
    let master_commit = get_master_commit store in
    master_commit >>== (fun master_commit ->
      try_with_commit (deliver_blob store) master_commit store)

  let delete_mail store path =
    let master_commit = get_master_commit store in
    master_commit >>== (fun master_commit ->
      try_with_commit (Raw.delete_mail store path) master_commit store)

  let move_mail store path new_path =
    let master_commit = get_master_commit store in
    master_commit >>== (fun master_commit ->
      try_with_commit (Raw.move_mail store path new_path) master_commit store)

  let add_mail_time time store path input =
    let path = Git.Path.v (Fpath.to_string path) in
    let blob_hash = add_blob_to_store store input in
    let add_blob_time time store path commit =
      let tree = get_commit_tree store commit in
      blob_hash
      |> lwt_result_bind2 (fun a b -> add_blob_to_tree_extend store a path b) tree
      >>== commit_tree ~time store [commit] "deliver mail" in
    let master_commit = get_master_commit store in
    master_commit >>== (fun master_commit ->
      try_with_commit (add_blob_time time store path) master_commit store)

  let add_mail = add_mail_time (Unix.time ())

  let init_gitmaildir store =
    init_empty_blob store
end
