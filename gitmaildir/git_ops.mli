module type S = sig

  (** The Store module used to make the implementation. *)
  module Store : Git.Store.S

  (** The type of an error *)
  type error = [
    | `Not_a_tree of Store.Hash.t
    | `Not_a_commit of Store.Hash.t
    | `Not_a_blob of Store.Hash.t
    | `No_entry_in_tree of string * Store.Value.Tree.t
    | `Invalid_path of Git.Path.t
    | Store.error ]

  (** Pretty prints error type *)
  val pp_error : Format.formatter -> error -> unit

  (** Add new blob to store *)
  val add_blob_to_store : Store.t -> in_channel -> (Store.Hash.t, error) Lwt_result.t

  (** Commit the given tree with the given parent *)
  val commit_tree : ?time:float -> Store.t -> Store.Hash.t list -> string -> Store.Hash.t 
                    ->(Store.Hash.t, error) Lwt_result.t

  (** Modifies subtree at path using given function *)
  val modify_tree : Store.t -> Store.Hash.t -> Git.Path.t
                    -> f:(Store.Value.Tree.t -> Store.Value.Tree.t)
                    -> (Store.Hash.t, error) Lwt_result.t

  (** Inserts hash at point given by path in tree (will fail if intermediate dirs do not exist) *)
  val add_blob_to_tree : Store.t -> Store.Hash.t -> Git.Path.t -> Store.Hash.t
                        -> (Store.Hash.t, error) Lwt_result.t

  (** Inserts hash at point in tree, creating intermediate subdirs *)
  val add_blob_to_tree_extend : Store.t -> Store.Hash.t -> Git.Path.t -> Store.Hash.t
                        -> (Store.Hash.t, error) Lwt_result.t

  (** Removes file at path from the tree (fails in same way as add_hash_to_tree) *)
  val remove_entry_from_tree : Store.t -> Store.Hash.t -> Git.Path.t
                               -> (Store.Hash.t, error) Lwt_result.t

  (** makes a ref at from the given path *)
  val make_ref : Git.Path.t -> Git.Reference.t

  (** Follows reference until returning the final hash pointed to *)
  val hash_of_ref : Store.t -> Git.Reference.t -> (Store.Hash.t, error) Lwt_result.t

  (** Updates the ref to the given hash in the store *)
  val update_ref : Store.t -> Git.Reference.t -> Store.Hash.t -> (unit, error) Lwt_result.t

  (** Get git commit at head of master *)
  val get_master_commit : Store.t -> (Store.Hash.t, error) Lwt_result.t

  (** Get git commit at head of reference *)
  val get_branch_commit : Store.t -> Git.Reference.t -> (Store.Hash.t, error) Lwt_result.t

  (** Returns a list of the parents of the given commit *)
  val get_commit_parents : Store.t -> Store.Hash.t -> (Store.Hash.t list, error) Lwt_result.t

  (** Returns the top level tree of the given commit *)
  val get_commit_tree : Store.t -> Store.Hash.t -> (Store.Hash.t, error) Lwt_result.t

  (** Returns the hash for object at given path (relative to root of given tree *)
  val get_hash_at_path : Store.t -> Store.Hash.t -> Git.Path.t
                         -> (Store.Hash.t, error) Lwt_result.t

  (** creates a single commit of an empty blob in a new git directory *)
  val init_empty_blob : Store.t -> (unit, error) Lwt_result.t

  (** Checkout a blob to the location specified in path *)
  val read_blob : Store.t -> Store.Hash.t -> (string, error) Lwt_result.t 
end

module Make (Store : Git.Store.S) : sig
  include
    S
    with module Store = Store
end
