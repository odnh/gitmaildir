open Core

type t = File_entry of (int * string)
       | Dir_entry of t list

let diff_trees a b = ([], [])

let tree_of_path _ = File_entry (0, "")

module Store = Git_unix.Store

let tree_of_git_path _ _ = File_entry (0, "")
