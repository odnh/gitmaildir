open Core

(** the type of a filetree *)
type t

(** returns a list of all the elements only in the first tree and all those only in the second *)
val diff_trees : t -> t -> (string list * string list)

(** creates a filetree from a path in the filesystem *)
val tree_of_path : Fpath.t -> t

(** creates a filetree from a git store *)
val tree_of_git_path : Fpath.t -> Fpath.t -> t
