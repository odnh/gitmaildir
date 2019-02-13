(** Implements very simple locking which works on Mac/Linux/BSD. Uses flock as the backend for interprocess with OCaml Mutex for intraprocess *)

(** the type of a lock *)
type t

(** create a lock(file) *)
val v : string -> t

(** try to lock the given lock (blocks until success) *)
val lock : t -> unit

(** unlock the given lock *)
val unlock : t -> unit
