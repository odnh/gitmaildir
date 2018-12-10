type git_store 
(** The type of a git store (need to update from ocaml-git) **)

type git_tree
(** The type of a git tree **)

type git_hash
(** The type of a git hash **)

val hash_object: git_store -> in_channel -> git_hash
(** perform a git hash-object on given input and store **)

val cat_file: git_store -> git_hash -> in_channel
(** perform git cat-file and return contents as an in_channel **)

val mktree: git_store -> git_tree -> git_hash
(** perform git mktree **)

val get_head_tree: git_store -> git_tree
(** returns the tree at the HEAD tag of the given repository **)

val add_to_tree: git_tree -> string -> git_hash -> git_tree
(** add the given hash to the given git tree with filename **)

val commit_tree: git_store -> git_hash -> git_hash
(** perform git commit_tree operation **)

val update_head: git_store -> git_hash -> unit
(** updates refs/heads/master to the given hash in the git store **)

val store_of_string: string -> git_store
(** convert a string into a git_store type **)

val tree_of_string: string -> git_tree
(** convert a string into a git_tree type **)

val hash_of_string: string -> git_hash
(** convert a string to a git_hash **)
