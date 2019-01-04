open Git_unix

(** Return git store for given file path *)
val store_of_string : string -> (Store.t, Store.error) result Lwt.t

(** Add new blob to store *)
val add_blob_to_store : Store.t -> in_channel -> (Store.Hash.t * int, Store.error) result Lwt.t

(** Get top level git tree for reference *)
val get_ref_tree : Store.t -> Store.Reference.t -> Store.Hash.t

(** Inserts hash at point given by path in tree *)
val add_hash_to_tree: Store.t -> Store.Value.Tree.t -> Git.Path.t -> Store.Hash.t
                      -> (Store.Value.Tree.t, string) result Lwt.t
