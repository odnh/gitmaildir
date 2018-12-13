(** The type of a git store *)
type git_store 

(** The type of a git tree *)
type git_tree

(** The type of a git hash *)
type git_hash

(** perform a git hash-object on given input and store *)
val hash_object: git_store -> in_channel -> git_hash

(** perform git cat-file and return contents as an in_channel *)
val cat_file: git_store -> git_hash -> in_channel

(** perform git mktree *)
val mktree: git_store -> git_tree -> git_hash

(** returns the tree at the HEAD tag of the given repository *)
val get_head_tree: git_store -> git_tree

(** add the given hash to the given git tree with filename *)
val add_to_tree: git_tree -> string -> git_hash -> git_tree

(** perform git commit_tree operation *)
val commit_tree: git_store -> git_hash -> git_hash

(** updates refs/heads/master to the given hash in the git store *)
val update_head: git_store -> git_hash -> unit

(** convert a string into a git_store type *)
val store_of_string: string -> git_store

(** convert a string into a git_tree type *)
val tree_of_string: string -> git_tree

(** convert a string to a git_hash *)
val hash_of_string: string -> git_hash

(** removes the given hash from the given tree. NB no impact on store *)
val remove_from_tree: git_tree -> git_hash -> git_tree

(** returns a hash for a file name in the current tree *)
val hash_from_filename: git_store -> string -> git_hash option
