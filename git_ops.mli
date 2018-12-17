open Git_unix

(** Return git store for given file path *)
val store_of_string : string -> (Store.t, Store.error) result Lwt.t

(** Add new blob to store *)
val add_to_store : Store.t-> in_channel -> (Digestif.SHA1.t * int, Store.error) result Lwt.t

(** Adds the blob to the tree *)
(*val add_blob_to_tree : Store.t -> Fpath.t -> Store.Hash.t *)
