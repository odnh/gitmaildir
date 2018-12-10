open Core

type t
(** the type of a maildir **)

val deliver_mail: Git_ops.git_store -> In_channel.t -> unit
(** delivers a new mail to the specified git store TODO: return something useful **)
