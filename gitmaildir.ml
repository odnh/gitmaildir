open Core

let action_param =
  let open Command.Param in
  anon ("action" %: string)

let deliver () =
    let store = Git_ops.store_of_string
      "/Users/oliver/Google Drive/Cambridge/CST_II/project/testing/gt" in
    let store_no_err = match Lwt_main.run store with Ok s -> s | Error _ -> failwith "ERR" in
    let _ = Git_ops.add_to_store store_no_err In_channel.stdin in
    ()

let take_action action = match action with
  | "deliver" -> print_endline "Delivering"; deliver ()
  | "delete" -> print_endline "Deleting"
  | other -> print_endline ("No action for: " ^ other)

let command =
  Command.basic
    ~summary:"Interact with git maildir"
    ~readme:(fun () -> "More detailed info")
    (Command.Param.map action_param ~f:(fun action ->
        (fun () -> take_action action)))

let () =
  Command.run ~version:"0.1" command;
