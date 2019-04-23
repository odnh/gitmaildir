open Core

module Make_unsafe (G : Git_ops.S) = struct
  
  (* TODO: use a passed in standard maildir functor to do all the work, and just modify the input on the delivery bit (as well as the branch used to get the reference of course *)

  module Store = G.Store
  module Raw = Maildir.Make_raw(G)
  open G

  (** TODO: do something with the input channel *)
  let deliver_mail store input =
    let plain_ref = Git.Reference.of_path Git.Path.(v plain) in
    let plain_commit = get_branch_commit store plain_ref in
    plain_commit >>== Raw.deliver_mail store input
    >>== update_ref store plain_ref

  let delete_mail store path =
    let plain_ref = Git.Reference.of_path Git.Path.(v plain) in
    let plain_commit = get_branch_commit store plain_ref in
    plain_commit >>== Raw.delete_mail store path
    >>== update_ref store plain_ref

  let move_mail store path new_path =
    let plain_ref = Git.Reference.of_path Git.Path.(v plain) in
    let plain_commit = get_branch_commit store plain_ref in
    plain_commit >>== Raw.move_mail store path new_path
    >>== update_ref store plain_ref

  let add_mail_time time store path input =
    let plain_ref = Git.Reference.of_path Git.Path.(v plain) in
    let plain_commit = get_branch_commit store plain_ref in
    plain_commit >>== Raw.add_mail_time time store path input
    >>== update_ref store plain_ref

  let add_mail = add_mail_time (Unix.time ())

  let init_gitmaildir store =
    init_empty_blob store
end
