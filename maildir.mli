open Core

(** the type of a maildir *)
type t

(** delivers a new mail to the specified git store *)
val deliver_mail: Git_ops.git_store -> In_channel.t -> unit

(*
(** move (ie rename) and email TODO: support having directory hierarchy *)
val move_mail: Git_ops.git_store -> string -> string -> unit

(** removes the given mail from the current tree TODO: support a directory hierarchy *)
val delete_mail: Git_ops.git_store -> string -> unit
*)
