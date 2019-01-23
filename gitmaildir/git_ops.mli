module type S = sig

  type error

  (** The Store module used to make the implementation. *)
  module Store : Git.Store.S

  (** Add new blob to store *)
  val add_blob_to_store : Store.t -> in_channel -> (Store.Hash.t, error) result Lwt.t

  (** Commit the given tree with the given parent *)
  val commit_tree : ?time:float -> Store.t -> Store.Hash.t list -> string -> Store.Hash.t 
                    ->(Store.Hash.t, error) result Lwt.t

  (** Modifies subtree at path using given function *)
  val modify_tree : Store.t -> Store.Hash.t -> Git.Path.t
                    -> f:(Store.Value.Tree.t -> Store.Value.Tree.t)
                    -> (Store.Hash.t, error) result Lwt.t

  (** Inserts hash at point given by path in tree (will fail if intermediate dirs do not exist) *)
  val add_blob_to_tree : Store.t -> Store.Hash.t -> Git.Path.t -> Store.Hash.t
                        -> (Store.Hash.t, error) result Lwt.t

  (** Inserts hash at point in tree, creating intermediate subdirs *)
  val add_blob_to_tree_extend : Store.t -> Store.Hash.t -> Git.Path.t -> Store.Hash.t
                        -> (Store.Hash.t, error) result Lwt.t

  (** Removes file at path from the tree (fails in same way as add_hash_to_tree) *)
  val remove_entry_from_tree : Store.t -> Store.Hash.t -> Git.Path.t
                               -> (Store.Hash.t, error) result Lwt.t

  (** makes a ref at from the given path *)
  val make_ref : Git.Path.t -> Git.Reference.t

  (** Follows reference until returning the final hash pointed to *)
  val hash_of_ref : Store.t -> Git.Reference.t -> (Store.Hash.t, error) result Lwt.t

  (** Updates the ref to the given hash in the store *)
  val update_ref : Store.t -> Git.Reference.t -> Store.Hash.t -> (unit, error) result Lwt.t

  (** Get git commit at head of master *)
  val get_master_commit : Store.t -> (Store.Hash.t, error) result Lwt.t

  (** Get git commit at head of reference *)
  val get_branch_commit : Store.t -> Git.Reference.t -> (Store.Hash.t, error) result Lwt.t

  (** Returns a list of the parents of the given commit *)
  val get_commit_parents : Store.t -> Store.Hash.t -> (Store.Hash.t list, error) result Lwt.t

  (** Returns the top level tree of the given commit *)
  val get_commit_tree : Store.t -> Store.Hash.t -> (Store.Hash.t, error) result Lwt.t

  (** Returns the hash for object at given path (relative to root of given tree *)
  val get_hash_at_path : Store.t -> Store.Hash.t -> Git.Path.t
                         -> (Store.Hash.t, error) result Lwt.t

  (** Replaces the contents of the given path with the tree at the given commit *)
  val checkout_to_dir : Store.t -> Store.Hash.t -> Fpath.t -> (unit, error) result Lwt.t

  (** creates a single commit of an empty blob in a new git directory *)
  val init_empty_blob : Store.t -> (unit, error) result Lwt.t
end

module Make (Store : Git.Store.S) : sig
  include
    S
    with module Store = Store
end
