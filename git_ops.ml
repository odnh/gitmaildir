open Core
open Git_unix
open Lwt.Infix

let store_of_string path =
  Fpath.v path
  |> Store.v

let add_blob_to_store store input =
  In_channel.input_all input
  |> Store.Value.Blob.of_string 
  |> Store.Value.blob
  |> Store.write store

(*let get_ref_tree store ref = need to work out how to turn a ref into something useful *)

(*let commit_tree store tree message =
  let author = in (* need to work out how to create a Git.User *)
  let commiter = in (* same here *)
  Store.Value.Commit.make ~tree ~author: ~commiter: message*)

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

let add_hash_to_tree (store:Store.t) (tree:Store.Hash.t) (path:Git.Path.t) (hash:Store.Hash.t) : Store.Hash.t option Lwt.t =
  let module Tree = Store.Value.Tree in
  match get_last @@ Git.Path.segs path with
  | None -> Lwt.return_none
  | Some (name, loc) ->
    let new_entry = make_tree_entry hash name in
    let rec aux (path:string list) (tree_hash:Store.Hash.t) : Store.Hash.t option Lwt.t =
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
