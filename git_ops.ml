open Core
open Git_unix
open Lwt.Infix

(* -------------------- Helper functions ------------------- *)

(** Returns a pair of a list and the last element in it *)
let lwt_option_map a b =
  Lwt.map (fun x -> match x with Some x -> Some (a x) | None -> None) b

let lwt_option_bind a b =
  Lwt.map (fun x -> match x with Some x -> a x | None -> None) b

let (>>>|) a b = lwt_option_map b a

let (>>>=) a b = lwt_option_bind b a

let option_pair = function
  | Some a, Some b -> Some (a, b)
  | _ -> None

let get_last l =
  let rev = List.rev l in
  (List.hd rev, Option.map (List.tl rev) ~f:List.rev)
  |> option_pair

(** returns and Ok Tree if is a tree and Error otherwise *)
let read_as_tree store hash =
  let data = Store.read store hash in
  let match_tree = function
    | Ok (Store.Value.Tree t) -> Some t
    | _ -> None
  in
  Lwt.map match_tree data

let entry_from_tree name tree =
  Store.Value.Tree.to_list tree
    |> List.find ~f:(fun e -> e.Store.Value.Tree.name = name)

let make_tree_entry hash name =
  Store.Value.Tree.entry name `Normal hash

let store_write_option store value =
  Store.write store value
  >|= (function
    | Ok (h, _) -> Some h
    | _ -> None)

let get_user = fun () ->
  { Git.User.name = "gitmaildir";
    Git.User.email = "gitmaildir@localhost";
    Git.User.date = (Unix.time () |> Int64.of_float, None) }

(* -------------------- Main Functions -------------------- *)

let store_of_string path =
  Fpath.v path
    |> Store.v
  >|= Result.ok

let add_blob_to_store store input =
  In_channel.input_all input
    |> Store.Value.Blob.of_string 
  |> Store.Value.blob
  |> Store.write store
  >|= Result.ok
  >>>| (function (h, _ ) -> h)

let add_hash_to_tree store tree path hash =
  let module Tree = Store.Value.Tree in
  match get_last @@ Git.Path.segs path with
  | None -> Lwt.return_none
  | Some (name, loc) ->
    let new_entry = make_tree_entry hash name in
    let rec aux path tree_hash =
      match path with
      | [] ->
        read_as_tree store tree_hash
        >>>| (fun t -> Tree.add t new_entry)
        >>>| Store.Value.tree
        >>= (fun v -> match v with
          | Some v -> store_write_option store v
          | None -> Lwt.return_none)
      | x::xs ->
        let curr_tree = read_as_tree store tree_hash in
        let subtree_entry = curr_tree >>>= entry_from_tree x in
        let new_subtree_hash = subtree_entry 
        >>= (function
          | Some e -> aux xs e.Tree.node
          | None -> Lwt.return_none) in
          let new_subtree_entry = subtree_entry
        >>= (fun e -> match e with
          | Some e -> new_subtree_hash >>>| Tree.entry e.Tree.name e.Tree.perm
          | None -> Lwt.return_none) in
          let new_tree = curr_tree
        >>>| Tree.remove ~name:x
        >>= (fun t -> match t with
          | Some t -> new_subtree_entry >>>| Tree.add t
          | None -> Lwt.return None) in
          new_tree >>>| Store.Value.tree
        >>= (function
          | Some v -> Store.write store v >|= (function
            | Ok (h, _) -> Some h
            | _ -> None)
          | None -> Lwt.return_none) in
    aux loc tree

(* TODO: major clean up, this has way too much duplication *)
let remove_entry_from_tree store tree path =
  let module Tree = Store.Value.Tree in
  match get_last @@ Git.Path.segs path with
  | None -> Lwt.return_none
  | Some (name, loc) ->
    let rec aux path tree_hash =
      match path with
      | [] ->
        read_as_tree store tree_hash
        >>>| Tree.remove ~name
        >>>| Store.Value.tree
        >>= (fun v -> match v with
          | Some v -> store_write_option store v
          | None -> Lwt.return_none)
      | x::xs ->
        let curr_tree = read_as_tree store tree_hash in
        let subtree_entry = curr_tree >>>= entry_from_tree x in
        let new_subtree_hash = subtree_entry 
        >>= (function
          | Some e -> aux xs e.Tree.node
          | None -> Lwt.return_none) in
          let new_subtree_entry = subtree_entry
        >>= (fun e -> match e with
          | Some e -> new_subtree_hash >>>| Tree.entry e.Tree.name e.Tree.perm
          | None -> Lwt.return_none) in
          let new_tree = curr_tree
        >>>| Tree.remove ~name:x
        >>= (fun t -> match t with
          | Some t -> new_subtree_entry >>>| Tree.add t
          | None -> Lwt.return None) in
          new_tree >>>| Store.Value.tree
        >>= (function
          | Some v -> Store.write store v >|= (function
            | Ok (h, _) -> Some h
            | _ -> None)
          | None -> Lwt.return_none) in
    aux loc tree

let commit_tree store parent message tree =
  let user = get_user () in
  Store.Value.Commit.make ~tree:tree ~author:user
    ~committer:user message ~parents:[parent]
  |> Store.Value.commit
  |> Store.write store
  >|= Result.ok
  >>>| (function (h, _ ) -> h)

let hash_of_ref store ref =
  let rec aux ref =
    Store.Ref.read store ref
    >>= (function
      | Ok a -> (match a with
        | Hash h -> Lwt.return_some h
        | Ref r -> aux r)
      | Error _ -> Lwt.return_none) in
  aux ref

let update_ref store ref hash =
  let contents : Store.Reference.head_contents = Hash hash in
  Store.Ref.write store ref contents
  >|= Result.ok

let get_master_commit store =
  hash_of_ref store Git.Reference.master

let get_commit_parents store commit =
  Store.read store commit
  >|= Result.ok
  >>>= (function
    | Commit c -> Some c 
    | _ -> None)
  >>>| Store.Value.Commit.parents

let get_commit_tree store commit =
  Store.read store commit
  >|= Result.ok
  >>>= (function
    | Commit c -> Some c 
    | _ -> None)
  >>>| Store.Value.Commit.tree

let hash_of_path store tree path =
  let module Tree = Store.Value.Tree in
  match get_last @@ Git.Path.segs path with
  | None -> Lwt.return_none
  | Some (name, loc) ->
    let rec aux path tree_hash =
      match path with
      | [] ->
        read_as_tree store tree_hash
        >>>| Tree.to_list
        >>>= List.find ~f:(fun e -> e.Tree.name = name)
        >>>| (fun e -> e.Tree.node)
      | x::xs ->
        let curr_tree = read_as_tree store tree_hash in
        let subtree_entry = curr_tree >>>= entry_from_tree x in
        subtree_entry >>= (function
          | Some e -> aux xs e.Tree.node
          | None -> Lwt.return_none) in
    aux loc tree
