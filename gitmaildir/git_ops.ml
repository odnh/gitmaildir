open Core
open Lwt.Infix
open Lwt_result_helpers

module type S = sig

  type error

  module Store : Git.Store.S

  val add_blob_to_store : Store.t -> In_channel.t -> (Store.Hash.t, error) result Lwt.t

  val commit_tree : ?time:float -> Store.t -> Store.Hash.t list -> string -> Store.Hash.t 
                    ->(Store.Hash.t, error) result Lwt.t

  val modify_tree : Store.t -> Store.Hash.t -> Git.Path.t
                    -> f:(Store.Value.Tree.t -> Store.Value.Tree.t)
                    -> (Store.Hash.t, error) result Lwt.t

  val add_blob_to_tree : Store.t -> Store.Hash.t -> Git.Path.t -> Store.Hash.t
                        -> (Store.Hash.t, error) result Lwt.t

  val add_blob_to_tree_extend : Store.t -> Store.Hash.t -> Git.Path.t -> Store.Hash.t
                        -> (Store.Hash.t, error) result Lwt.t

  val remove_entry_from_tree : Store.t -> Store.Hash.t -> Git.Path.t
                               -> (Store.Hash.t, error) result Lwt.t

  val make_ref : Git.Path.t -> Git.Reference.t

  val hash_of_ref : Store.t -> Git.Reference.t -> (Store.Hash.t, error) result Lwt.t

  val update_ref : Store.t -> Git.Reference.t -> Store.Hash.t -> (unit, error) result Lwt.t

  val get_master_commit : Store.t -> (Store.Hash.t, error) result Lwt.t

  val get_branch_commit : Store.t -> Git.Reference.t -> (Store.Hash.t, error) result Lwt.t

  val get_commit_parents : Store.t -> Store.Hash.t -> (Store.Hash.t list, error) result Lwt.t

  val get_commit_tree : Store.t -> Store.Hash.t -> (Store.Hash.t, error) result Lwt.t

  val get_hash_at_path : Store.t -> Store.Hash.t -> Git.Path.t
                         -> (Store.Hash.t, error) result Lwt.t

  val checkout_to_dir : Store.t -> Store.Hash.t -> Fpath.t -> (unit, error) result Lwt.t

  val init_empty_blob : Store.t -> (unit, error) result Lwt.t
end

module Make (Store : Git.Store.S) = struct

  module Store = Store
  module Tree = Store.Value.Tree

  type error = [
    | `Not_a_tree
    | `Not_a_commit
    | `No_entry_in_tree
    | `Invalid_path
    | Store.error ]

  (** for lifting Store.error up to Git.ops.error *)
  let lift_error err = (err :> error)

  (** Returns the last item of a list and the rest of the list *)
  let get_last l =
    let rev = List.rev l in
    (List.hd rev, Option.map (List.tl rev) ~f:List.rev)
    |> option_pair

  (** returns an error unless value is a tree *)
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

  (** returns a default user with current time for commits *)
  let get_user time =
    { Git.User.name = "gitmaildir";
      Git.User.email = "gitmaildir@localhost";
      Git.User.date = (time |> Int64.of_float, None) }

  (** wraps Store.write to return larger error *)
  let write_value store value =
    Store.write store value
    >|= Result.map_error ~f:lift_error
    >|= Result.map ~f:(fst)

  (** wraps Store.read to return larger error *)
  let read_value store value =
    Store.read store value
    >|= Result.map_error ~f:lift_error

  let add_blob_to_store store input =
    In_channel.input_all input
      |> Store.Value.Blob.of_string
      |> Store.Value.blob
      |> write_value store

  let modify_tree store tree path ~f =
    let path_segs = Git.Path.segs path in
    let rec aux path tree_hash =
      match path with
      | [] ->
          read_as_tree store tree_hash
          >>|| f
          >>|| Store.Value.tree
          >>== write_value store
      | x::xs ->
          let current_tree = read_as_tree store tree_hash in
          let subtree_entry = current_tree >>=| entry_from_tree x in
          let new_subtree_hash = subtree_entry
          >>= (function
            | Ok e -> aux xs e.Tree.node
            | Error e -> Lwt.return_error e) in
          let new_subtree_entry = subtree_entry
          >>= (function
            | Ok e -> new_subtree_hash >>|| Tree.entry e.Tree.name e.Tree.perm
            | Error e -> Lwt.return_error e) in
          let new_tree = current_tree
          >>|| Tree.remove ~name:x
          >>= (function
            | Ok t -> new_subtree_entry >>|| Tree.add t
            | Error e -> Lwt.return_error e) in
          new_tree >>|| Store.Value.tree
          >>= (function
            | Ok v -> write_value store v
            | Error e ->  Lwt.return_error e) in
    aux path_segs tree

  let get_hash_at_path store tree path =
    let path_segs = Git.Path.segs path in
    let rec aux path tree_hash =
      match path with
      | [] -> Lwt.return_ok tree_hash
      | x::xs ->
          let current_tree = read_as_tree store tree_hash in
          let subtree_entry = current_tree >>=| entry_from_tree x in
          subtree_entry
          >>|| (fun e -> e.Tree.node)
          >>== aux xs in
    aux path_segs tree

  let add_blob_to_tree store tree path blob =
    match get_last @@ Git.Path.segs path with
    | None -> Lwt.return_error `Invalid_path
    | Some (name, loc) ->
        let entry = Tree.entry name `Normal blob in
        modify_tree store tree (Git.Path.of_segs loc) ~f:(fun t -> Tree.add t entry)

  (* Returns a hash of a tree all the way down to a blob at the given path *)
  let rec build_subtrees store path name blob = match path with
    | [] ->
        let entry = Tree.entry name `Normal blob in
        let tree = Tree.of_list [entry] in
        Store.Value.tree tree
        |> write_value store
    | x::xs ->
        let subtree = build_subtrees store xs name blob in
        let entry = subtree >>|| Tree.entry x `Dir in
        let tree = entry >>|| (fun e -> Tree.of_list [e]) in
        tree >>|| Store.Value.tree
        >>== write_value store

  let add_blob_to_tree_extend store tree path blob =
    let rec aux (name, loc) tree_hash = match loc with
      | [] ->
          let entry = Tree.entry name `Normal blob in
          read_as_tree store tree_hash
          >>|| (fun t -> Tree.add t entry)
          >>|| Store.Value.tree
          >>== write_value store
      | x::xs ->
          let current_tree = read_as_tree store tree_hash in
          let subtree_entry = current_tree >>=| entry_from_tree x in
          subtree_entry >>= function
            | Error `No_entry_in_tree ->
                let new_subtree_hash = build_subtrees store xs name blob in
                let new_subtree_entry =
                  new_subtree_hash >>|| Tree.entry x `Dir in
                let new_tree = current_tree
                >>= (function
                  | Ok t -> new_subtree_entry >>|| Tree.add t
                  | Error e -> Lwt.return_error e) in
                new_tree >>|| Store.Value.tree
                >>= (function
                  | Ok v -> write_value store v
                  | Error e ->  Lwt.return_error e)
            | Error e -> Lwt.return_error e
            | Ok entry ->
                let new_subtree_hash = aux (name, xs) entry.Tree.node in
                let new_subtree_entry =
                  new_subtree_hash >>|| Tree.entry entry.Tree.name entry.Tree.perm in
                let new_tree = current_tree
                >>|| Tree.remove ~name:x
                >>= (function
                  | Ok t -> new_subtree_entry >>|| Tree.add t
                  | Error e -> Lwt.return_error e) in
                new_tree >>|| Store.Value.tree
                >>= (function
                  | Ok v -> write_value store v
                  | Error e ->  Lwt.return_error e) in
    match get_last @@ Git.Path.segs path with
    | None -> Lwt.return_error `Invalid_path
    | Some p -> aux p tree

  let remove_entry_from_tree store tree path =
    match get_last @@ Git.Path.segs path with
    | None -> Lwt.return_error `Invalid_path
    | Some (name, loc) ->
        modify_tree store tree (Git.Path.of_segs loc) ~f:(Tree.remove ~name)

  let commit_tree ?time:(time = Unix.time ()) store parents message tree  =
    let user = get_user time in
    let message = "\n" ^ message ^ "\n" in
    Store.Value.Commit.make ~tree:tree ~author:user
      ~committer:user message ~parents:parents
    |> Store.Value.commit
    |> write_value store

  let make_ref path =
    Git.Reference.of_path path

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

  let get_branch_commit store branch_ref =
    hash_of_ref store branch_ref

  let get_commit_parents store commit =
    read_value store commit
    >>=| (function
      | Commit c -> Ok c
      | _ -> Error `Not_a_commit)
    >>|| Store.Value.Commit.parents

  let get_commit_tree store commit =
    read_value store commit
    >>=| (function
      | Commit c -> Ok c
      | _ -> Error `Not_a_commit)
    >>|| Store.Value.Commit.tree

  (* TODO: implement *)
  let checkout_to_dir _ _ _ =
    Lwt.return_ok ()

  let init_empty_blob store =
    let blob = Store.Value.Blob.of_string "" |> Store.Value.blob in
    write_value store blob
    >>|| (fun h -> Tree.of_list [Tree.entry ".keep" `Normal h])
    >>|| Store.Value.tree
    >>== write_value store
    >>== commit_tree store [] "init"
    >>== update_ref store Git.Reference.master
end
