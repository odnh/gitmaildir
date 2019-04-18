open Core
open Gitmaildir.Lwt_result_helpers

module Store = Git.Store.Make(Digestif.SHA1)(Git_unix.Fs)(Git.Inflate)(Git.Deflate)
module Git_ops = Gitmaildir.Git_ops.Make(Store)
module Lock = Gitmaildir_unix.Locking_unix
module Maildir = Gitmaildir.Maildir.Make_granular(Git_ops)(Locking_unix)

let lock = Lock.v ".global_lock"
(*let get_lock store = Lock.v ((Fpath.to_string @@ Store.root store) ^ "/.global_lock")*)

(*

let read_git_tree store hash =
  (* Read git tree, return entries *)
  Store.read store hash >>|| (function
    | Ok (Store.Value.Tree t) -> Ok t 
    | _ -> Error (`Not_a_tree hash)) >>||
  Store.Value.Tree.to_list

let read_dir_tree root path =
  (* Read dir in as list, (w name and whether file/folder) *)
  let full_path = root ^ "/" ^ path in
  let dir_contents = Sys.readdir full_path |> Array.to_list |> List.map ~f:(f -> path ^ "/" ^ f) in
  let is_dir = List.map dir_contents ~f:(fun f -> if Sys.is_directory_exn (full_path ^ "/" ^ f) then (f,`Dir) else (f,`File)) in
  is_dir

(* Helper funtions for finding difference between lists *)
let tree_elem_in_dir elem dir = List.fold dir ~init:false ~f:(fun a f -> a || Store.Value.Tree.node elem = (fst f))
let dir_elem_in_tree elem tree = List.fold tree ~init:false ~f:(fun a f -> a || Store.Value.Tree.node f = (fst elem))

let read_dir_recurse root path =
  (* returns all elements below path in maildir at root *)
  let full_path = root ^ "/" ^ path in
  let dir_contents = Sys.readdir full_path |> Array.to_list |> List.map ~f:(f -> path ^ "/" ^ f) in
  let dirs = List.filter dir_contents ~f:(fun f -> Sys.is_directory_exn (full_path ^ "/" ^ f)) in
  List.map dirs ~f:(read_dir_recurse root) |> List.concat |> List.append dir_contents

let read_git_recurse store hash path =
  (* returns all elements below tree hash in gitmaildir store, paired with their path *)
  let entries = Store.read store hash >>|| (function
    | Ok (Store.Value.Tree t) -> Ok t 
    | _ -> Error (`Not_a_tree hash)) >>||
  Store.Value.Tree.to_list in
  let subtrees = entries >>|| List.filter ~f:(fun e ->
    match Store.Value.Tree.perm e with
    | `Dir -> true
    | _ -> false)
  let entries_with_path = entries >>|| List.map ~f:(fun e -> (path, e)) in
  List.map subtrees ~f:(fun e ->
    read_git_recurse store (Store.Value.Tree.node e) (path ^ "/" ^ (Store.Value.Tree.name e)))
  |> List.concat |> List.append entries_with_path

let diff_trees store root hash path =
  (* General idea:
    * Call recursively on dirs and hashes ie:
      * On find difference, add to list, similarity don't
      * If that similarity is a dir/tree then recurse
      * If the difference is a dir/tree, call different func returning all contents *)
  let git_tree = read_git_tree store hash in
  let dir_tree = read_dir_tree root path in
  let unique_git = List.filter git_tree ~f:(fun x -> not (tree_elem_in_dir x dir_tree) in
  let unique_dir = List.filter dir_tree ~f:(fun x -> not (dir_elem_in_tree x git_tree) in
  let shared_subdirs = List.filter git_tree ~f:(WRITE FUNC) in
  (* TODO: deal with shared & unique dirs *)

  (* TODO: take into account git history for deletions *)
*)

(* New idea: create data struct of both, diff func just works on these, read in and read out done separately *)
(* Tree format: list of file or named sublist *)

type file_tree = File of string | Dir of string * (file_tree list)

(** If passed a diir, returns its name, otherwise raises an exception *)
let dir_name = function
  | Dir (n, l) -> n
  | File _ -> failwith "Not a dir"

(** Returns a list of all the Files and a list of all the Dirs in a file_tree *)
val ft_split : file_tree list -> file_tree list -> (file_tree list) * (file_tree list)
let ft_split ft =
  match ft with
  | File f -> ([],[f])
  | Dir (n, l) ->
      let files = List.filter l ~f:(function Dir _ -> false | File _ -> true) in
      let dirs = List.filter l ~f:(function Dir _ -> true | File _ -> false) in
      (dirs, files)

(** Returns the name associated with the top level of the file tree *)
val get_ft_name : file_tree -> string
let get_ft_name =
  match ft with
  | File f -> f
  | Dir (s, _) -> s


val git_to_maildir_write : string -> file_tree list -> unit
let git_to_maildir_write path files =

val delete_in_maildir : string -> file_tree list -> unit
let delete_in_maildir path files =

val maildir_to_git_write : string -> file_tree list -> unit
let maildir_to_git_write path files =

(** Returns the unique entries of each list, (must be lists of file_trees) *)
val unique_entries : file_tree list -> file_tree list -> (file_tree list) * (file_tree list)
let unique_entries l1 l2 =
  let is_member elem l =
    match elem with
    | Dir (n1, l1) ->
        List.fold l ~init:false ~f:(fun acc x -> match x with Dir (n2,l2) -> (n1 = n2) || acc | File f -> acc)
    | File f1 ->
        List.fold l ~init:false ~f:(fun acc x -> match x with Dir _ -> acc | File f2 -> (f1 = f2) || acc) in
  let unique_l1 = List.filter l1 ~f:(fun x -> not (is_member x l2)) in
  let unique_l2 = List.filter l2 ~f:(fun x -> not (is_member x l2)) in
  (unique_l1, unique_l2)

(** Returns the shared dir entries in a list of file_trees along with the dir name *)
val shared_dir_entries : file_tree list -> file_tree list -> (file_tree * file_tree * string) list
let shared_dir_entries l1 l2 =
  let get_member elem l =
    match elem with
    | Dir (n1, l1) ->
        List.fold l ~init:None ~f:(fun acc x -> 
          match x with
          | Dir (n2, l2) -> if (n1 = n2) then (Some (Dir (n1, l1))) || acc
          | File f -> acc)
    | File f1 ->
        List.fold l ~init:None ~f:(fun acc x ->
          match x with
          | Dir _ -> acc
          | File f2 -> if (f1 = f2) then (Some (File f2)) || acc) in
  let shared = List.fold l1 ~init:[] ~f:(
    fun acc elem ->
      match get_member elem l2 with
      | None -> acc
      | Some elem2 -> (elem, elem2, get_ft_name elem)::acc) in
  shared

(** Reads a git store in as a file_tree (given a commit) *)
val file_tree_of_git : Store.t -> Store.Hash.t -> (file_tree, Store.error) * Lwt_result.t
let rec file_tree_of_git store commit =
  module SVT = Store.Value.Tree in
  let rec aux hash =
    let entries = Store.read store hash >>|| (function
      | Ok (Store.Value.Tree t) -> Ok t
      | _ -> Error (`Not_a_tree hash)) >>||
      SVT.to_list in
    let subtrees = entries >>|| List.filter ~f:(fun e ->
      match SVT.perm e with
      | `Dir -> true
      | _ -> false) in
    let blobs = entries >>|| List.filter ~f:(fun e ->
      match SVT.perm e with
      | `Normal -> true
      | _ -> false )in
    let files_wrapped = blobs >>|| List.map ~f:(fun f -> File (SVT.name f)) in
    List.map subtrees ~f:(fun s -> Dir ((SVT.name s), (aux (SVT.node s)))) |> List.append files_wrapped in
  let commit_tree_hash = Git_ops.get_commit_tree store commit in
  commit_tree_hash >>|| aux >>|| (fun x -> Dir ("", x))

(** Reads a maildir in as a file_tree *)
val file_tree_of_maildir : string -> string -> file_tree
let rec file_tree_of_maildir root =
  let rex aux path =
    let full_path = root ^ "/" ^ path in
    let dir_contents = Sys.readdir full_path |> Array.to_list in
    let dirs = List.filter dir_contents ~f:(fun f -> Sys.is_directory_exn (full_path ^ "/" ^ path ^ "/" ^ f)) in
    let files = List.filter dir_contents ~f:(fun f -> not (Sys.is_directory_exn (full_path ^ "/" ^ path ^ "/" ^ f))) in
    let files_wrapped = List.map files ~f:(fun f -> File f) in
    List.map dirs ~f:(fun d -> Dir (d, aux (path ^ "/" ^ d))) |> List.append files_wrapped in
  "" |> aux |> (fun x -> Dir ("", x))

(** Takes a git file_tree and maildir file_tree, and synchronises any differences *)
val diff_trees : file_tree -> file_tree -> string list
let rec diff_trees git_tree maildir_tree =
  let aux path gt mt =
    let git_dirs, git_files = ft_split git_tree in
    let maildir_dirs, maildir_files = ft_split maildir_tree in
    (* Sort out files *)
    let unique_git_f, unique_maildir_f = unique_entries git_files maildir_files in
    let delete_in_maildir_f, put_in_git_f = in_prev_commit unique_maildir_f in
    git_to_maildir_write path unique_git_f;
    delete_in_maildir path delete_in_maildir_f;
    maildir_to_git_write path put_in_git_f;

    (* Sort out dirs *)
    let unique_git_d, unique_maildir_d = unique_entries git_dirs maildir_dirs in
    git_to_maildir_write path unique_git_d;
    delete_in_maildir path delete_in_maildir_d;
    maildir_to_git_write path put_in_git_d;

    (* Recursive step *)
    let shared_dirs = shared_dir_entries git_dirs maildir_dirs in(*TODO: write func*)
    List.iter shared_dirs ~f:(fun (a,b,name) -> aux (path ^ "/" ^ name) a b) in
  aux "" git_tree maildir_tree

let add_to_gitmaildir_fold ~store errors file =
  (* Adds contents of file to store or bypasses if error exists *)
  errors >>== fun (() ->
  In_channel.create file |> fun (input ->
  Maildir.add_mail store file input |> fun (result ->
  In_channel.close input;
  result (* May have problem of closing input before it's actually used *)

let add_to_maildir_fold ~store errors file =
  (* Adds contents of hash at path: 'file' to maildir or bypasses if error exists *)
  errors >>== fun(() ->
  In_channel.create file |> fun (in
  (* TODO: complete *)


let sync store dir =
  Lock.lock lock;
  let (new_git, new_maildir, del_git, del_maildir) = diff_trees store dir in
  List.fold new_maildir errors ~f:(add_to_gitmaildir_fold ~store)
  List.fold new_git errors ~f:(add_to_maildir ~store)
  Lock.unlock lock

let run_daemon store dir = ()
  (* TODO run synchronisation *)
  sync store dir;
  (* TODO then enter filesystem watching (see https://github.com/ocaml/dune/blob/master/src/scheduler.ml) *)
