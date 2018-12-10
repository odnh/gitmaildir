open Core

let action_param =
  let open Command.Param in
  anon ("action" %: string)

let deliver () =
  let st = Git_ops.store_of_string "/Users/oliver/Google Drive/Cambridge/CST_II/project/testing/gt/.git" in
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
