open Core
open Lwt.Infix
open Gitmaildir.Lwt_result_helpers

module Store = Git.Store.Make(Digestif.SHA1)(Git_unix.Fs)(Git.Inflate)(Git.Deflate)
module Git_ops = Gitmaildir.Git_ops.Make(Store)
module Lock = Gitmaildir_unix.Locking_unix
module Maildir = Gitmaildir.Maildir.Make_granular(Git_ops)(Lock)

let lock = Lock.v ".global_lock"
(*let get_lock store = Lock.v ((Fpath.to_string @@ Store.root store) ^ "/.global_lock")*)

type error = [
  | `Not_a_tree of Store.Hash.t
  | Git_ops.error ]

type file_tree = File of string | Dir of string * (file_tree list)

(** If passed a diir, returns its name, otherwise raises an exception *)
let dir_name = function
  | Dir (n, _) -> n
  | File _ -> failwith "Not a dir"

(** Returns a list of all the Files and a list of all the Dirs in a file_tree *)
let ft_split ft =
  match ft with
  | File f -> ([],[File f])
  | Dir (_, l) ->
      let files = List.filter l ~f:(function Dir _ -> false | File _ -> true) in
      let dirs = List.filter l ~f:(function Dir _ -> true | File _ -> false) in
      (dirs, files)

(** Returns the name associated with the top level of the file tree *)
let get_ft_name ft =
  match ft with
  | File f -> f
  | Dir (s, _) -> s

(** Takes a file_tree and returns a list of all the paths contained within *)
let file_tree_to_paths ft =
  let rec aux ft =
    match ft with
    | File f -> [f]
    | Dir (n, l) -> List.map l ~f:aux |> List.concat |> List.map ~f:(fun s -> n ^ "/" ^ s) in
  aux ft

(** Returns the unique entries of each list, (must be lists of file_trees) *)
let unique_entries l1 l2 =
  let is_member elem l =
    match elem with
    | Dir (n1, _) ->
        List.fold l ~init:false ~f:(fun acc x -> match x with Dir (n2, _) -> (n1 = n2) || acc | File _ -> acc)
    | File f1 ->
        List.fold l ~init:false ~f:(fun acc x -> match x with Dir _ -> acc | File f2 -> (f1 = f2) || acc) in
  let unique_l1 = List.filter l1 ~f:(fun x -> not (is_member x l2)) in
  let unique_l2 = List.filter l2 ~f:(fun x -> not (is_member x l2)) in
  (unique_l1, unique_l2)

(** Returns the shared dir entries in a list of file_trees *)
let shared_entries l1 l2 =
  let get_member elem l =
    match elem with
    | Dir (n1, l1) ->
        List.fold l ~init:None ~f:(fun acc x -> 
          match x with
          | Dir (n2, _) -> if (n1 = n2) then (Some (Dir (n1, l1))) else acc
          | File _ -> acc)
    | File f1 ->
        List.fold l ~init:None ~f:(fun acc x ->
          match x with
          | Dir _ -> acc
          | File f2 -> if (f1 = f2) then (Some (File f2)) else acc) in
  let shared = List.fold l1 ~init:[] ~f:(
    fun acc elem ->
      match get_member elem l2 with
      | None -> acc
      | Some elem2 -> (elem, elem2)::acc) in
  shared

(** Reads a git store in as a file_tree (given a commit) *)
let file_tree_of_git store commit =
  let module SVT = Store.Value.Tree in
  let rec aux (hash : Digestif.SHA1.t) : (file_tree sexp_list, error) result Lwt.t =
    let entries = Store.read store hash >|= (function
      | Ok (Store.Value.Tree t) -> Ok t
      | _ -> Error (`Not_a_tree hash)) >>||
      SVT.to_list in
    let subtrees = entries >>|| List.filter ~f:(fun e ->
      match e.SVT.perm with
      | `Dir -> true
      | _ -> false) in
    let blobs = entries >>|| List.filter ~f:(fun e ->
      match e.SVT.perm with
      | `Normal -> true
      | _ -> false )in
    let files_wrapped = blobs >>|| List.map ~f:(fun f -> File (f.SVT.name)) in
    let subtrees_ft = subtrees >>=| fun st ->
      let module Err = struct exception Aux_err of error end in
      try Ok (List.map st ~f:(fun s ->
        let recursive_result = Lwt_main.run (aux s.SVT.node) in
        (match recursive_result with
          | Ok a -> Dir ((s.SVT.name), a)
          | Error e -> raise (Err.Aux_err e))))
      with Err.Aux_err e -> Error e in
    files_wrapped >>== (fun files_wrapped ->
      subtrees_ft >>|| (fun subtrees_ft ->
        List.append files_wrapped subtrees_ft)) in
  let commit_tree_hash = Git_ops.get_commit_tree store commit in
  commit_tree_hash >>== aux >>|| (fun x -> Dir ("", x))

(** Reads a maildir in as a file_tree *)
let file_tree_of_maildir root =
  let rec aux path =
    let full_path = root ^ "/" ^ path in
    let dir_contents = Sys.readdir full_path |> Array.to_list in
    let dirs = List.filter dir_contents ~f:(fun f -> Sys.is_directory_exn (full_path ^ "/" ^ path ^ "/" ^ f)) in
    let files = List.filter dir_contents ~f:(fun f -> not (Sys.is_directory_exn (full_path ^ "/" ^ path ^ "/" ^ f))) in
    let files_wrapped = List.map files ~f:(fun f -> File f) in
    List.map dirs ~f:(fun d -> Dir (d, aux (path ^ "/" ^ d))) |> List.append files_wrapped in
  "" |> aux |> (fun x -> Dir ("", x))

(** Takes two file_trees and returns a file_tree for each tree of its unique elements **)
let diff_trees tree_a tree_b =
  let rec aux tree_a tree_b =
    (* prepare data *)
    let dir_name = get_ft_name tree_a in
    let a_files, a_dirs = ft_split tree_a in
    let b_files, b_dirs = ft_split tree_b in
    (* diff data *)
    let unique_a_files, unique_b_files = unique_entries a_files b_files in
    let unique_a_dirs, unique_b_dirs = unique_entries a_dirs b_dirs in
    let shared_dirs = shared_entries a_dirs b_dirs in
    (* act on subdirs *)
    let recursed_dirs = List.map shared_dirs ~f:(fun (a, b) -> aux a b) in
    let recursed_a = List.map recursed_dirs ~f:(fun (a, _) -> a) in
    let recursed_b = List.map recursed_dirs ~f:(fun (_, b) -> b) in
    (* compile and return results *)
    let a_list = unique_a_files @ unique_a_dirs @ recursed_a in
    let b_list = unique_b_files @ unique_b_dirs @ recursed_b in
    let diff = (Dir (dir_name, a_list), Dir (dir_name, b_list)) in
    diff
  in
  aux tree_a tree_b

(** Returns true if paths exists in tree for parent of given commit, false otherwise *)
let path_exists_in_prev_commit store commit path =
  Git_ops.get_commit_parents store commit
  >>=| (fun parents -> List.hd  parents |> function Some h -> Ok h | None -> Error `Not_found)
  >>== Git_ops.get_commit_tree store
  >>== (fun tree -> Git_ops.get_hash_at_path store tree Git.Path.(v path))
  >|= (function
       | Ok _ -> true
       | Error _ -> false)
  |> Lwt_main.run

(** Transfer items listed in file tree to git store from maildir store *)
let sync_maildir_to_git git_store commit maildir_path ft =
  let sync_path path =
    In_channel.create (maildir_path ^ "/" ^ path) |> fun input ->
    Maildir.add_mail git_store (Fpath.v (maildir_path ^ "/" ^ path)) input |> fun lwt ->
    let _ = Lwt_main.run lwt in
    In_channel.close input in
  let delete_path path =
    Sys.remove (maildir_path ^ "/" ^ path) in
  let paths = file_tree_to_paths ft in
  let to_delete = List.filter paths ~f:(path_exists_in_prev_commit git_store commit) in
  let to_sync = List.filter paths ~f:(fun elem1 -> List.exists to_delete ~f:(fun elem2 -> elem1 = elem2)) in
  (* delete unneeded *)
  List.iter to_delete ~f:delete_path;
  (* sync new emails *)
  List.iter to_sync ~f:sync_path

(** Transfer items listed in file_tree to maildir store from git store, but choosing to delete depending on git history *)
let sync_git_to_maildir git_store commit maildir_path ft =
  let sync_path path =
    let gpath = Git.Path.v path in
    let lwt_result_string = Git_ops.get_commit_tree git_store commit >>==
      (fun tree -> Git_ops.get_hash_at_path git_store tree gpath) >>==
      (fun hash -> Git_ops.read_blob git_store hash) in
    let file = Out_channel.create (maildir_path ^ "/" ^ path) in
      (match Lwt_main.run lwt_result_string with
      | Ok s -> Out_channel.output_string file s
      | Error _ -> ());
      Out_channel.close file in
  let paths = file_tree_to_paths ft in
  List.iter paths ~f:sync_path

(** Synchronises a git store and maildir (takes and holds the global lock) *)
let sync git_store maildir_path =
  Lock.lock lock;
  let maildir_path = Fpath.to_string maildir_path in
  let head_commit = Lwt_main.run @@ Git_ops.get_master_commit git_store in
  let g_tree = Result.bind head_commit ~f:(fun c -> Lwt_main.run @@ file_tree_of_git git_store c) in
  let m_tree = Ok (file_tree_of_maildir maildir_path) in
  let unique_pair = Result.bind g_tree ~f:(fun g_tree ->
    Result.map m_tree ~f:(fun m_tree ->
      diff_trees g_tree m_tree)) in
  let _ = Result.bind head_commit ~f:(fun c ->
    Result.map unique_pair ~f:(fun (m, g) ->
      sync_maildir_to_git git_store c maildir_path m;
      sync_git_to_maildir git_store c maildir_path g)) in
  Lock.unlock lock

(** Uses fswatch to listen on events on the given path and executes the function handed to it *)
let fswatch_event_listen path f =
  let act_on_event ic =
    let rec loop () =
      try let _ = In_channel.input_line_exn ic in f (); loop ()
      with End_of_file -> () in
    loop () in
  let command = "fswatch -r " ^ path ^ " --event Created --event Updated --event Removed" in
  let input = Unix.open_process_in command in
  act_on_event input;
  let _ = Unix.close_process_in input in
  ()

let run_daemon git_store maildir_path =
  sync git_store maildir_path;
  fswatch_event_listen Fpath.(to_string maildir_path) (fun () -> sync git_store maildir_path)
