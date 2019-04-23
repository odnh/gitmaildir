open Core
open Lwt.Infix
open Lwt_result_helpers

let make_plain data = data

module Make (G : Git_ops.S) = struct
  
  (* TODO: use a passed in standard maildir functor to do all the work, and just modify the input on the delivery bit (as well as the branch used to get the reference of course *)

  module Store = G.Store
  module Raw = Maildir.Make_raw(G)
  open G

  (** TODO: do something with the input channel *)
  let deliver_mail store input =
    let data = In_channel.input_all input in
    let plain_data = make_plain data in
    let temp_file = Filename.temp_file "gitmaildir" "plain_deliver" in
    let oc = Out_channel.create temp_file in
    Out_channel.output_string oc plain_data;
    Out_channel.close oc;
    let ic = In_channel.create temp_file in
    let plain_ref = Git.Reference.of_path Git.Path.(v "plain") in
    let plain_commit = get_branch_commit store plain_ref in
    plain_commit >>== Raw.deliver_mail store ic
    >>== update_ref store plain_ref >|= (fun a ->
    In_channel.close ic;
    Sys.remove temp_file;
    a)

  let delete_mail store path =
    let plain_ref = Git.Reference.of_path Git.Path.(v "plain") in
    let plain_commit = get_branch_commit store plain_ref in
    plain_commit >>== Raw.delete_mail store path
    >>== update_ref store plain_ref

  let move_mail store path new_path =
    let plain_ref = Git.Reference.of_path Git.Path.(v "plain") in
    let plain_commit = get_branch_commit store plain_ref in
    plain_commit >>== Raw.move_mail store path new_path
    >>== update_ref store plain_ref

  let add_mail_time time store path input =
    let plain_ref = Git.Reference.of_path Git.Path.(v "plain") in
    let plain_commit = get_branch_commit store plain_ref in
    plain_commit >>== Raw.add_mail_time time store path input
    >>== update_ref store plain_ref

  let add_mail = add_mail_time (Unix.time ())

  let init_gitmaildir store =
    init_empty_blob store
end
