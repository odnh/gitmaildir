(** The type of a module implementing locking *)
module type Locking = sig
  (** the type of a lock *)
  type t

  (** create a lock(file) *)
  val v : string -> t

  (** try to lock the given lock (blocks until success) *)
  val lock : t -> unit

  (** unlock the given lock *)
  val unlock : t -> unit
end

(** Type that allows us to pass unmade functors into the plugin functor *)
module type Makeable = functor (G : Git_ops.S) ->
sig
  include
    Maildir.S
    with module Store = G.Store
    and type error := G.error
end

(** Adds a plugin to an existing maildir module. NB: can stack these on top of eachother. *)
module Make_unsafe (M : Makeable) (P : Makeable) (G : Git_ops.S) : sig
  include
    Maildir.S
    with module Store = G.Store
    and type error := G.error
end

(** Wrap a pluginned maildir to with non-granular locking *)
module Make_locking (M : Makeable) (P : Makeable) (L : Locking) (G : Git_ops.S) : sig
  include
    Maildir.S
    with module Store = G.Store
    and type error := G.error
end

(** Takes a maildir implementation and a list of plugins to be added on top of it *)
val add_plugins : (module Makeable) -> (module Makeable) list -> (module Locking) -> (module Makeable)
