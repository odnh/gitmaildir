open Core
open Git_unix
open Lwt.Infix
open Lwt_result_helpers

type error = [
  | `Not_a_tree
  | `Not_a_commit
  | `No_entry_in_tree
  | `Invalid_path
  | Store.error ]

let lift_error err = (err :> error)

let get_last l =
  let rev = List.rev l in
  (List.hd rev, Option.map (List.tl rev) ~f:List.rev)
  |> option_pair

(** returns and Some Tree if is a tree and Error otherwise *)
let read_as_tree store hash =
  let data = Store.read store hash in
  let match_tree = (function
    | Ok (Store.Value.Tree t) -> Ok t
    | Error e -> Error (lift_error e)
    | _ -> Error `Not_a_tree) in
  data >|= match_tree

let entry_from_tree name tree =
  Store.Value.Tree.to_list tree
  |> List.find ~f:(fun e -> e.Store.Value.Tree.name = name)
  |> Result.of_option ~error:`No_entry_in_tree

(* returns a default user with current time for commits *)
let get_user = fun () ->
  { Git.User.name = "gitmaildir";
    Git.User.email = "gitmaildir@localhost";
    Git.User.date = (Unix.time () |> Int64.of_float, None) }

(** wraps Store.write to return larger error *)
let write_value store value =
  Store.write store value
  >|= Result.map_error ~f:lift_error
  >|= Result.map ~f:(fst)

(** wraps Store.read to return larger error *)
let read_value store value =
  Store.read store value
  >|= Result.map_error ~f:lift_error

let store_of_string path =
  Fpath.v path
    |> Store.v
    >|= Result.map_error ~f:lift_error

let add_blob_to_store store input =
  In_channel.input_all input
    |> Store.Value.Blob.of_string
    |> Store.Value.blob
    |> write_value store

let modify_tree store tree path ~f =
  let module Tree = Store.Value.Tree in
  let path_segs = Git.Path.segs path in
  let rec aux path tree_hash =
    match path with
    | [] ->
        read_as_tree store tree_hash
        >>>| f
        >>>| Store.Value.tree
        >>== write_value store
    | x::xs ->
        let current_tree = read_as_tree store tree_hash in
        let subtree_entry = current_tree >>>= entry_from_tree x in
        let new_subtree_hash = subtree_entry
        >>= (function
          | Ok e -> aux xs e.Tree.node
          | Error e -> Lwt.return_error e) in
        let new_subtree_entry = subtree_entry
        >>= (function
          | Ok e -> new_subtree_hash >>>| Tree.entry e.Tree.name e.Tree.perm
          | Error e -> Lwt.return_error e) in
        let new_tree = current_tree
        >>>| Tree.remove ~name:x
        >>= (function
          | Ok t -> new_subtree_entry >>>| Tree.add t
          | Error e -> Lwt.return_error e) in
        new_tree >>>| Store.Value.tree
        >>= (function
          | Ok v -> write_value store v
          | Error e ->  Lwt.return_error e) in
  aux path_segs tree

let build_subtrees store path hash =
  let module Tree = Store.Value.Tree in
  let rec aux = function
    | [] -> Lwt.return_error `Invalid_path
    | x::[] ->
        let entry = Tree.entry x `Normal hash in
        let tree = Tree.of_list [entry] in
        write_value store (tree |> Store.Value.tree)
    | x::xs ->
        let sub_tree = aux xs in
        let entry = sub_tree >>>| Tree.entry x `Normal in
        let tree = entry >>>| (fun e -> Tree.of_list [e]) in
        tree >>>| Store.Value.tree
        >>== write_value store in
  aux (Git.Path.segs path)

let get_remaining_path store tree path =
  let module Tree = Store.Value.Tree in
  let rec aux tree path = match path with
    | [] -> Lwt.return_ok Git.Path.empty
    | x::xs ->
        let tree_value = read_as_tree store tree in
        let entry = tree_value >>>= entry_from_tree x in
        entry >>= function
          | Error `No_entry_in_tree -> Lwt.return_ok (Git.Path.of_segs (x::xs))
          | Ok e -> aux e.Tree.node xs
          | Error e -> Lwt.return_error e in
  aux tree (Git.Path.segs path)
  
let get_hash_at_path store tree path =
  let module Tree = Store.Value.Tree in
  let path_segs = Git.Path.segs path in
  let rec aux path tree_hash =
    match path with
    | [] -> Lwt.return_ok tree_hash
    | x::xs ->
        let current_tree = read_as_tree store tree_hash in
        let subtree_entry = current_tree >>>= entry_from_tree x in
        subtree_entry
        >>>| (fun e -> e.Tree.node)
        >>== aux xs in
  aux path_segs tree

let add_hash_to_tree store tree path hash =
  let module Tree = Store.Value.Tree in
  match get_last @@ Git.Path.segs path with
  | None -> Lwt.return_error `Invalid_path
  | Some (name, loc) ->
      let remaining = get_remaining_path store tree (Git.Path.of_segs loc) in
      remaining >>== fun r ->
        if Git.Path.equal r Git.Path.empty then
          let entry = Tree.entry name `Normal hash in
          modify_tree store tree (Git.Path.of_segs loc) ~f:(fun t -> Tree.add t entry)
        else
          let subtree = build_subtrees store (Git.Path.add r name) hash in
          let last_dir = List.rev (Git.Path.segs r) |> List.hd_exn in
          let entry = subtree >>>| Tree.entry last_dir `Normal in
          entry >>== fun e ->
            modify_tree store tree (Git.Path.of_segs loc) ~f:(fun t -> Tree.add t e)

let remove_entry_from_tree store tree path =
  let module Tree = Store.Value.Tree in
  match get_last @@ Git.Path.segs path with
  | None -> Lwt.return_error `Invalid_path
  | Some (name, loc) ->
      modify_tree store tree (Git.Path.of_segs loc) ~f:(Tree.remove ~name)

let commit_tree store parent message tree =
  let user = get_user () in
  let message = "\n" ^ message ^ "\n" in
  Store.Value.Commit.make ~tree:tree ~author:user
    ~committer:user message ~parents:[parent]
  |> Store.Value.commit
  |> write_value store

let hash_of_ref store ref =
  let rec aux ref =
    Store.Ref.read store ref
    >>= (function
      | Ok a -> (match a with
        | Hash h -> Lwt.return_ok h
        | Ref r -> aux r)
      | Error e -> Lwt.return_error (lift_error e)) in
  aux ref

let update_ref store ref hash =
  let contents : Store.Reference.head_contents = Hash hash in
  Store.Ref.write store ref contents
  >|= Result.map_error ~f:(lift_error)

let get_master_commit store =
  hash_of_ref store Git.Reference.master

let get_commit_parents store commit =
  read_value store commit
  >>>= (function
    | Commit c -> Ok c
    | _ -> Error `Not_a_commit)
  >>>| Store.Value.Commit.parents

let get_commit_tree store commit =
  read_value store commit
  >>>= (function
    | Commit c -> Ok c
    | _ -> Error `Not_a_commit)
  >>>| Store.Value.Commit.tree

let checkout_to_dir _ _ _ =
    Lwt.return_ok ()
