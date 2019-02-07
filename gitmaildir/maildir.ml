open Core
open Lwt.Infix
open Lwt_result_helpers

(* -------------------- Helper functions -------------------- *)

let get_new_email_filename () =
  (string_of_int (int_of_float ((Unix.gettimeofday ()) *. 1_000_000.)))
  ^ "."
  ^ (string_of_int (Random.bits ())) (*TODO: make randomness more inline with official spec*)
  ^ "."
  ^ (Unix.gethostname ())

(* -------------------- Main functions -------------------- *)

module type Locking = sig
  type t

  val v : string -> t

  val lock : t -> unit

  val try_lock : t -> bool

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

  val convert_maildir : Store.t -> Fpath.t -> (unit, error) Lwt_result.t
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
    all_files |> List.map ~f:(fun f -> f, (Unix.stat f).st_mtime)
    |> List.sort ~compare:(fun (_,t1) (_,t2) -> Float.compare t1 t2)
    |> List.map ~f:(fun (f,t) ->
        (Fpath.(v f |> segs)
          |> (fun l -> List.drop l maildir_root_length)
          |> String.concat ~sep:Fpath.dir_sep
        , f, t))
    |> List.fold ~init:(Lwt.return_ok ()) ~f:(
      fun acc (f1, f2, t) ->
        (* TODO: make line below lwt so not too many simultaneous fds *)
        let input = In_channel.create f2 in
        acc >>== (fun () -> add_mail_time t store (Fpath.v f1) input)
        >>|| (fun x -> In_channel.close input; x))
end

module Make_locking (G : Git_ops.S) (L : Locking) = struct

  module Store = G.Store
  module Unsafe = Make_unsafe(G)
  module Lock = L
  open G

  let lock = Lock.v ".global_lock"

  let deliver_mail store input =
    Lwt.return_unit >|= (fun () ->
    Lock.lock lock) >>= (fun () ->
    Unsafe.deliver_mail store input) >|= (fun a ->
    Lock.unlock lock; a)

  let delete_mail store path =
    Lwt.return_unit >|= (fun () ->
    Lock.lock lock) >>= (fun () ->
    Unsafe.delete_mail store path) >|= (fun a ->
    Lock.unlock lock; a)

  let move_mail store path new_path =
    Lwt.return_unit >|= (fun () ->
    Lock.lock lock) >>= (fun () ->
    Unsafe.move_mail store path new_path) >|= (fun a ->
    Lock.unlock lock; a)

  let add_mail_time time store path input =
    Lwt.return_unit >|= (fun () ->
    Lock.lock lock) >>= (fun () ->
    Unsafe.add_mail_time time store path input) >|= (fun a ->
    Lock.unlock lock; a)

  let add_mail = add_mail_time (Unix.time ())

  let init_gitmaildir store =
    init_empty_blob store

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
    all_files |> List.map ~f:(fun f -> f, (Unix.stat f).st_mtime)
    |> List.sort ~compare:(fun (_,t1) (_,t2) -> Float.compare t1 t2)
    |> List.map ~f:(fun (f,t) ->
        (Fpath.(v f |> segs)
          |> (fun l -> List.drop l maildir_root_length)
          |> String.concat ~sep:Fpath.dir_sep
        , f, t))
    |> List.fold ~init:(Lwt.return_ok ()) ~f:(
      fun acc (f1, f2, t) ->
        (* TODO: make line below lwt so not too many simultaneous fds *)
        let input = In_channel.create f2 in
        acc >>== (fun () -> add_mail_time t store (Fpath.v f1) input)
        >>|| (fun x -> In_channel.close input; x))
end

(* TODO: implement the locking *)
module Make_lockless (G : Git_ops.S) (L : Locking) = struct

  module Store = G.Store
  module Raw = Make_raw(G)
  module Lock = L
  open G

  let _lock = Lock.v ".global_lock"

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
    all_files |> List.map ~f:(fun f -> f, (Unix.stat f).st_mtime)
    |> List.sort ~compare:(fun (_,t1) (_,t2) -> Float.compare t1 t2)
    |> List.map ~f:(fun (f,t) ->
        (Fpath.(v f |> segs)
          |> (fun l -> List.drop l maildir_root_length)
          |> String.concat ~sep:Fpath.dir_sep
        , f, t))
    |> List.fold ~init:(Lwt.return_ok ()) ~f:(
      fun acc (f1, f2, t) ->
        (* TODO: make line below lwt so not too many simultaneous fds *)
        let input = In_channel.create f2 in
        acc >>== (fun () -> add_mail_time t store (Fpath.v f1) input)
        >>|| (fun x -> In_channel.close input; x))
end
