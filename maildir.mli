open Core

(** the type of a maildir *)
type t

(** delivers a new mail to the specified git store *)
val deliver_mail : Git_unix.Store.t -> In_channel.t -> unit option Lwt.t

(** move (ie rename) an email *)
val move_mail : Git_unix.Store.t -> Fpath.t -> Fpath.t -> unit

(** removes the given mail from the current tree *)
val delete_mail : Git_unix.Store.t -> Fpath.t -> unit option Lwt.t
