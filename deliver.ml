open Core

let deliver () =
  let name = Maildir.get_new_email_filename () in
  let data = In_channel.input_all stdin in (* maybe move this and above call to toplevel*)
  Git
