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

let get_ref_tree store ref = (* need to work out how to turn a ref into something useful *)

(*let commit_tree store tree message =
  let author = in (* need to work out how to create a Git.User *)
  let commiter = in (* same here *)
  Store.Value.Commit.make ~tree ~author: ~commiter: message*)

(** Returns a pair of a list and the last element in it *)
let get_last l =
  let rev = List.rev l in
  (List.hd rev, List.tl rev Option.(>>|) List.rev)

let lwt_option_map a b =
  Lwt.map (fun x -> match x with Some x -> Some (a x) | None -> None) b

let lwt_option_bind a b =
  Lwt.map (fun x -> match x with Some x -> a x | None -> None) b
  
let (>>>|) a b = lwt_option_map b a

let (>>>=) a b = lwt_option_bind b a

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
    |> List.find ~f:(fun e -> e.name = x)
  
let add_hash_to_tree store tree path hash =
  let module Tree = Store.Value.Tree in
  let name, loc = get_last @@ Git.Path.segs path in
  let new_entry = Tree.entry name `Normal hash in
  let rec aux path tree_hash =
    match path with
    | [] ->
      read_as_tree store tree_hash
      >>>| (fun t -> Tree.add t new_entry)
      >>>| Store.Value.tree
      >>>| Store.write store
    | x::xs ->
      let curr_tree = read_as_tree store tree_hash in
      let next_tree = curr_tree >>>= entry_from_tree name in
      let processed_next_tree = next_tree
        >>>| (fun e -> Tree.entry e.name e.perm (aux xs e.node))
      let new_tree = curr_tree
        >>>| Tree.remove name:x
        >>>| (fun t -> Tree.add t processed_next_tree)
      in
      new_tree >>>| Store.Value.tree >>>| Store.write store
  in
  aux loc tree
