open Core

(** delivers a new mail to the specified git store *)
val deliver_mail : Git_unix.Store.t -> In_channel.t -> (unit, Git_ops.error) result Lwt.t

(** move (ie rename) an email *)
val move_mail : Git_unix.Store.t -> Fpath.t -> Fpath.t -> (unit, Git_ops.error) result Lwt.t

(** removes the given mail from the current tree *)
val delete_mail : Git_unix.Store.t -> Fpath.t -> (unit, Git_ops.error) result Lwt.t

(** adds an email at any specified path *)
val add_mail : Git_unix.Store.t -> Fpath.t -> In_channel.t -> (unit, Git_ops.error) result Lwt.t

(** converts an existing maildir to a gitmaildir *)
val convert_maildir : Git_unix.Store.t -> Fpath.t -> (unit, Git_ops.error) result Lwt.t
