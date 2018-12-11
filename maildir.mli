open Core

(** the type of a maildir *)
type t

(** delivers a new mail to the specified git store TODO: return something useful *)
val deliver_mail: Git_ops.git_store -> In_channel.t -> unit
