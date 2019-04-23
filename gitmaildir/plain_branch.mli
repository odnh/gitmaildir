(** Performs maildir operations but delivering into a separate branch as plaintext *)
module Make (G : Git_ops.S) : sig
  include
    Maildir.S
    with module Store = G.Store
    and type error := G.error
end
