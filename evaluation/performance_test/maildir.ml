open Core

let get_new_email_filename () =
  (string_of_int (int_of_float ((Unix.gettimeofday ()) *. 1_000_000.)))
  ^ "."
  ^ (string_of_int (Random.bits ()))
  ^ "."
  ^ (Unix.gethostname ())

let split_name name =
  let split_list = Str.split (Str.regexp "2,") name in
  (List.hd_exn split_list, List.hd_exn @@ List.tl_exn split_list)

let deliver input store =
  let data = In_channel.input_all input in
  let name = get_new_email_filename () in
  let path = store ^ "/tmp/" ^ name in
  let path2 = store ^ "/new/" ^ name in
  let oc = Out_channel.create path in
  Out_channel.output_string oc data;
  Out_channel.close oc;
  Unix.link ~target:path ~link_name:path2 ();
  Unix.unlink path

let move store old_mail new_mail =
  let old_path = store ^ "/cur/" ^ old_mail in
  let new_path = store ^ "/cur/" ^ new_mail in
  Unix.link ~target:old_path ~link_name:new_path ();
  Unix.unlink old_path

let delete store mail =
  let path = store ^ "/cur/" ^ mail in
  Unix.unlink path
