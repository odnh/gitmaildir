open Git_unix

type error

(** Return git store for given file path *)
val store_of_string : string -> (Store.t, error) result Lwt.t

(** Add new blob to store *)
val add_blob_to_store : Store.t -> in_channel -> (Store.Hash.t, error) result Lwt.t

(** Get top level git tree for reference *)
val get_master_commit : Store.t -> (Store.Hash.t, error) result Lwt.t

(** Commit the given tree with the given parent *)
val commit_tree : Store.t -> Store.Hash.t -> string -> Store.Hash.t -> (Store.Hash.t, error) result Lwt.t

(** Modifies subtree at path using given function *)
val modify_tree : Store.t -> Store.Hash.t -> Git.Path.t
                  -> f:(Store.Value.Tree.t -> Store.Value.Tree.t)
                  -> (Store.Hash.t, error) result Lwt.t

(** builds git trees down to the given path with the hash at the last position *)
val build_subtrees : Store.t -> Git.Path.t -> Store.Hash.t -> (Store.Hash.t, error) result Lwt.t

(** returns any remaining path not exisiting in store *)
val get_remaining_path : Store.t -> Store.Hash.t -> Git.Path.t -> (Git.Path.t, error) result Lwt.t

(** Inserts hash at point given by path in tree (will fail if intermediate dirs do not exist) *)
val add_blob_to_tree : Store.t -> Store.Hash.t -> Git.Path.t -> Store.Hash.t
                      -> (Store.Hash.t, error) result Lwt.t

(** Removes file at path from the tree (fails in same way as add_hash_to_tree) *)
val remove_entry_from_tree : Store.t -> Store.Hash.t -> Git.Path.t
                             -> (Store.Hash.t, error) result Lwt.t

(** Follows reference until returning the final hash pointed to *)
val hash_of_ref : Store.t -> Git.Reference.t -> (Store.Hash.t, error) result Lwt.t

(** Updates the ref to the given hash in the store *)
val update_ref : Store.t -> Git.Reference.t -> Store.Hash.t -> (unit, error) result Lwt.t

(** Returns a list of the parents of the given commit *)
val get_commit_parents : Store.t -> Store.Hash.t -> (Store.Hash.t list, error) result Lwt.t

(** Returns the top level tree of the given commit *)
val get_commit_tree : Store.t -> Store.Hash.t -> (Store.Hash.t, error) result Lwt.t

(** Returns the hash for object at given path (relative to root of given tree *)
val get_hash_at_path : Store.t -> Store.Hash.t -> Git.Path.t
                       -> (Store.Hash.t, error) result Lwt.t

(** Replaces the contents of the given path with the tree at the given commit *)
val checkout_to_dir : Store.t -> Store.Hash.t -> Fpath.t -> (unit, error) result Lwt.t
