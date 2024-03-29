open Core

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

(** The type of a module implementing a gitmaildir returning a commit hash *)
module type S_raw = sig

  module Store : Git.Store.S 

  type error

  (** delivers a new mail to the specified git store *)
  val deliver_mail : Store.t -> In_channel.t -> Store.Hash.t -> (Store.Hash.t, error) Lwt_result.t

  (** move (ie rename) an email *)
  val move_mail : Store.t -> Fpath.t -> Fpath.t -> Store.Hash.t -> (Store.Hash.t, error) Lwt_result.t

  (** removes the given mail from the current tree *)
  val delete_mail : Store.t -> Fpath.t -> Store.Hash.t -> (Store.Hash.t, error) Lwt_result.t

  (** adds an email at any specified path *)
  val add_mail_time : float -> Store.t -> Fpath.t -> In_channel.t -> Store.Hash.t -> (Store.Hash.t, error) Lwt_result.t
  
  (** adds an email at any specified path *)
  val add_mail : Store.t -> Fpath.t -> In_channel.t -> Store.Hash.t -> (Store.Hash.t, error) Lwt_result.t
end

(** The type of a module implementing a gitmaildir *)
module type S = sig

  module Store : Git.Store.S 

  type error

  (** delivers a new mail to the specified git store *)
  val deliver_mail : Store.t -> In_channel.t -> (unit, error) Lwt_result.t

  (** move (ie rename) an email *)
  val move_mail : Store.t -> Fpath.t -> Fpath.t -> (unit, error) Lwt_result.t

  (** removes the given mail from the current tree *)
  val delete_mail : Store.t -> Fpath.t -> (unit, error) Lwt_result.t

  (** adds an email at any specified path *)
  val add_mail_time : float -> Store.t -> Fpath.t -> In_channel.t -> (unit, error) Lwt_result.t
  
  (** adds an email at any specified path *)
  val add_mail : Store.t -> Fpath.t -> In_channel.t -> (unit, error) Lwt_result.t

  (** inits a gitmaildir (ie empty git repository with initial commit) *)
  val init_gitmaildir : Store.t -> (unit, error) Lwt_result.t
end

(** Maildir functionality returning a commit object *)
module Make_raw (G : Git_ops.S) : sig
  include
    S_raw
    with module Store = G.Store
    and type error := G.error
end

(** Maildir functionality, unsafe in concurrent situations *)
module Make_unsafe (G: Git_ops.S) : sig
  include
    S
    with module Store = G.Store
    and type error := G.error
end

(** Maildir functionality with locking *)
module Make_locking (G : Git_ops.S) (L : Locking) : sig
  include
    S 
    with module Store = G.Store
    and type error := G.error
end

(** Maildir functionality with granular locking *)
module Make_granular (G : Git_ops.S) (L : Locking) : sig
  include
    S 
    with module Store = G.Store
    and type error := G.error
end
