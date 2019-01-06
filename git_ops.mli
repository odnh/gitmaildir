open Git_unix

(** Return git store for given file path *)
val store_of_string : string -> (Store.t, Store.error) result Lwt.t

(** Add new blob to store *)
val add_blob_to_store : Store.t -> in_channel -> Store.Hash.t option Lwt.t

(** Get top level git tree for reference *)
val get_master_tree : Store.t -> Store.Hash.t option Lwt.t

(** Commit the given tree with the given parent *)
val commit_tree : Store.t -> Store.Hash.t -> Store.Hash.t -> string -> Store.Hash.t option Lwt.t

(** Inserts hash at point given by path in tree (will fail if intermediate dirs do not exist) *)
val add_hash_to_tree : Store.t -> Store.Hash.t -> Git.Path.t -> Store.Hash.t
                      -> Store.Hash.t option Lwt.t

(** Follows reference until returning the final hash pointed to *)
val hash_of_ref : Store.t -> Git.Reference.t -> Store.Hash.t option Lwt.t
