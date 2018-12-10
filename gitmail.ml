open Core

let action_param =
  let open Command.Param in
  anon ("action" %: string)

(*let deliver () =
    let store = Git_ops.store_of_string "/Users/oliver/Desktop/t/gt/.git" in
    let hash = Git_ops.hash_of_string "d10400c69ee7f4685b20fbae63ebb07ea2c16cee" in
    Git_ops.cat_file store hash |> In_channel.input_all
*)

(*let deliver () =
    let gitstr = "git --git-dir=/Users/oliver/Desktop/t/gt/.git hash-object -w --stdin" in
    let (ic,oc) = Unix.open_process gitstr in
    let data = In_channel.input_all In_channel.stdin in
    let _sig2 = Out_channel.output_string oc data in
    Out_channel.flush oc; (* This may not be needed *)
    Out_channel.close oc; (* This is necessary to send EOF to proc *)
    let Some(hash) = In_channel.input_char ic in
    let _sig = Unix.close_process (ic,oc) in
    String.make 1 hash
*)

let deliver () =
  let st = Git_ops.store_of_string "/Users/oliver/Desktop/t/gt/.git" in
  Maildir.deliver_mail st In_channel.stdin

let take_action action = match action with
  | "deliver" -> print_endline "Delivering"; deliver ()
  | "insert" -> print_endline "Insert"
  | other -> print_endline ("No action for: " ^ other)

let command =
  Command.basic
    ~summary:"Interact with git maildir"
    ~readme:(fun () -> "More detailed info")
    (Command.Param.map action_param ~f:(fun action ->
        (fun () -> take_action action)))

let () =
  Command.run ~version:"0.1" command;
