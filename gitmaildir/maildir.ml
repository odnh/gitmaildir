open Core
open Git_ops
open Lwt_result_helpers

(* -------------------- Helper functions -------------------- *)

let get_new_email_filename () =
  (string_of_int (int_of_float ((Unix.gettimeofday ()) *. 1_000_000.)))
  ^ "."
  ^ (string_of_int (Random.bits ())) (*TODO: make randomness more inline with official spec*)
  ^ "."
  ^ (Unix.gethostname ())

(* -------------------- Main functions -------------------- *)

let deliver_mail store input =
  let mail_name = Git.Path.v @@ ("new/" ^ get_new_email_filename ()) in
  let master_ref = Git.Reference.master in
  let master_commit = get_master_commit store in
  let master_tree = master_commit >>== get_commit_tree store in
  add_blob_to_store store input
  |> lwt_result_bind2 (fun a b -> add_blob_to_tree_extend store a mail_name b) master_tree
  |> lwt_result_bind2 (fun a b -> commit_tree store [a] "deliver mail" b) master_commit
  >>== update_ref store master_ref

let delete_mail store path =
  let gpath = Git.Path.v (Fpath.to_string path) in
  let master_ref = Git.Reference.master in
  let master_commit = get_master_commit store in
  let master_tree = master_commit >>== get_commit_tree store in
  master_tree >>== (fun a -> remove_entry_from_tree store a gpath)
  |> lwt_result_bind2 (fun a b -> commit_tree store [a] "remove mail" b) master_commit
  >>== update_ref store master_ref

let move_mail store path new_path =
  let path = Git.Path.v (Fpath.to_string path) in
  let new_path = Git.Path.v (Fpath.to_string new_path) in
  let master_ref = Git.Reference.master in
  let master_commit = get_master_commit store in
  let master_tree = master_commit >>== get_commit_tree store in
  let hash = master_tree >>== (fun t -> get_hash_at_path store t path) in
  master_tree >>== (fun a -> remove_entry_from_tree store a path)
  |> lwt_result_bind2 (fun a b -> add_blob_to_tree store b new_path a) hash
  |> lwt_result_bind2 (fun a b -> commit_tree store [a] "move mail" b) master_commit
  >>== update_ref store master_ref

let add_mail_time time store path input =
  let path = Git.Path.v (Fpath.to_string path) in
  let master_ref = Git.Reference.master in
  let master_commit = get_master_commit store in
  let master_tree = master_commit >>== get_commit_tree store in
  add_blob_to_store store input
  |> lwt_result_bind2 (fun a b -> add_blob_to_tree_extend store a path b) master_tree
  |> lwt_result_bind2 (fun a b -> commit_tree ~time store [a] "deliver mail" b) master_commit
  >>== update_ref store master_ref

let add_mail = add_mail_time (Unix.time ())

let init_gitmaildir store =
  Git_ops.init_empty_blob store

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
      let input = In_channel.create f2 in
      acc >>== (fun () -> add_mail_time t store (Fpath.v f1) input)
      >>>| (fun x -> In_channel.close input; x))
