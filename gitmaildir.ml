open Core

let action_param =
  let open Command.Param in
  anon ("action" %: string)

let deliver () =
  let store = Git_ops.store_of_string
    "/Users/oliver/Google Drive/Cambridge/CST_II/project/testing/gt" in
  let store_no_err = match Lwt_main.run store with Some s -> s | None -> failwith "ERR" in
  match Lwt_main.run @@ Maildir.deliver_mail store_no_err In_channel.stdin with
  | Some _ -> ()
  | None -> failwith "ERR"

let delete path =
  let store = Git_ops.store_of_string
    "/Users/oliver/Google Drive/Cambridge/CST_II/project/testing/gt" in
  let store_no_err = match Lwt_main.run store with Some s -> s | None -> failwith "ERR" in
  match Lwt_main.run @@ Maildir.delete_mail store_no_err (Fpath.v path) with
  | Some _ -> ()
  | None -> failwith "ERR"

let take_action action = match action with
  | "deliver" -> print_endline "Delivering"; deliver ()
  | "delete" -> print_endline "Deleting"; delete "1546860539669981.994326685.Olivers-MacBook-Pro.local" 
  | other -> print_endline ("No action for: " ^ other)

let command =
  Command.basic
  ~summary:"Interact with git maildir"
  ~readme:(fun () -> "More detailed info")
  (Command.Param.map action_param ~f:(fun action ->
    (fun () -> take_action action)))

let () =
  Command.run ~version:"0.1" command;
