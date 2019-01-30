(** the type of a lock *)
type t

(** create a lock(file) *)
val v : string -> t

(** try to lock the given lock (blocks until success) *)
val lock : t -> unit

(** try to lock the given lock (None on failure) *)
val try_lock : t -> bool

(** unlock the given lock *)
val unlock : t -> unit
