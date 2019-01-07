open Git_unix

(** Return git store for given file path *)
val store_of_string : string -> Store.t option Lwt.t

(** Add new blob to store *)
val add_blob_to_store : Store.t -> in_channel -> Store.Hash.t option Lwt.t

(** Get top level git tree for reference *)
val get_master_commit : Store.t -> Store.Hash.t option Lwt.t

(** Commit the given tree with the given parent *)
val commit_tree : Store.t -> Store.Hash.t -> string -> Store.Hash.t -> Store.Hash.t option Lwt.t

(** Inserts hash at point given by path in tree (will fail if intermediate dirs do not exist) *)
val add_hash_to_tree : Store.t -> Store.Hash.t -> Git.Path.t -> Store.Hash.t
                      -> Store.Hash.t option Lwt.t

(** Removes file at path from the tree (fails in same way as add_hash_to_tree) *)
val remove_entry_from_tree : Store.t -> Store.Hash.t -> Git.Path.t -> Store.Hash.t option Lwt.t

(** Follows reference until returning the final hash pointed to *)
val hash_of_ref : Store.t -> Git.Reference.t -> Store.Hash.t option Lwt.t

(** Updates the ref to the given hash in the store *)
val update_ref : Store.t -> Git.Reference.t -> Store.Hash.t -> unit option Lwt.t

(** Returns a list of the parents of the given commit *)
val get_commit_parents : Store.t -> Store.Hash.t -> Store.Hash.t list option Lwt.t

(** Returns the top level tree of the given commit *)
val get_commit_tree : Store.t -> Store.Hash.t -> Store.Hash.t option Lwt.t

(** Returns the hash for object at given path *)
val hash_of_path : Store.t -> Git.Path.t -> Store.Hash.t option Lwt.t
