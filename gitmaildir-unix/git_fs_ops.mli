(** A set of git operations that only make sense in the context of a separate filesystem **)

module Store = Git_unix.Store

(** checkout a git store to a directory **)
val checkout_to_dir : Store.t -> Store.Hash.t -> Fpath.t -> unit Lwt.t
