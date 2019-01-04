open Core
open Git_unix
(*open Lwt.Infix*)

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

let rec add_hash_to_tree tree path hash =
  let module Tree = Store.Value.Tree in
  let tree_list = Tree.to_list tree in
  let mod_mode name path entry =
    if entry.name = name then
        let subtree = ... in
        add_hash_to_tree subtree path hash
        |> make a hash
    else
      entry
  match Git.Path.segs path with
  | p::[] -> Tree.entry p `Normal hash |> Tree.add tree
  | p::ps -> List.map ~f:(mod_node p ps) tree_list
             |> Tree.of_list
  | _ ->
