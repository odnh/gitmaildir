(** the type of an email *)
type t

(** create a modifiable mail *)
val v : in_channel -> t

(** converts an email to plaintext (removes attachments and body formatting) *)
val to_plaintext : t -> t

(** true if in same reply chain, false otherwise *)
val in_same_chain : t -> t -> bool
