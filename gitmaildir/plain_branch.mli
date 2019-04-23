(** Performs maildir operations but delivering into a separate branch as plaintext *)
module Make_unsafe (G : Git_ops.S) : sig
  include
    Maildir_with_plugins.Plugin
    with module Store = G.Store
    and type error := G.error
end
