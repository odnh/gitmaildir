open Core
open Lwt.Infix
open Lwt_result_helpers

(** Converts and html document to plain text *)
let html_to_plain html =
  html

(** takes in an email string and returns it as just plaintext content *)
let make_plain data =
  let module EE = Email_message.Email in
  let rec textify email =
    let parse_multi cont =
      match EE.Content.parse cont with
      | Ok parsed ->
        (match parsed with
        | EE.Content.Multipart m ->
            List.map m.EE.Content.Multipart.parts ~f:textify |> String.concat
        | EE.Content.Message m -> textify m
        | EE.Content.Data _ -> "")
      | Error _ -> "" in
    let parse_multi_alt cont =
      match EE.Content.parse cont with
      | Ok parsed ->
        (match parsed with
        | EE.Content.Multipart m ->
            (* As they are alternatives, we look for text/plain, otherwise choose the first one *)
            m.EE.Content.Multipart.parts
            |> List.fold ~init:(List.hd_exn m.EE.Content.Multipart.parts) ~f:(fun acc part ->
              let mimetype = EE.Simple.Content.of_email part |> EE.Simple.Content.content_type in
              if mimetype = "text/plain" then part else acc)
            |> textify
        | EE.Content.Message m -> textify m
        | EE.Content.Data _ -> "")
      | Error _ -> "" in
    let parse_text cont =
      match EE.Content.parse cont with
      | Ok parsed ->
        (match parsed with
        | EE.Content.Multipart _ -> ""
        | EE.Content.Message m -> textify m
        | EE.Content.Data d ->
            (match Email_message.Octet_stream.decode d with
            | Some s -> Email_message.Bigstring_shared.to_string s
            | None -> ""))
      | Error _ -> "" in
    let parse_html cont = 
      match EE.Content.parse cont with
      | Ok parsed ->
        (match parsed with
        | EE.Content.Multipart _ -> ""
        | EE.Content.Message m -> textify m
        | EE.Content.Data d ->
            (match Email_message.Octet_stream.decode d with
            | Some s -> Email_message.Bigstring_shared.to_string s |> html_to_plain
            | None -> ""))
      | Error _ -> "" in
    let content = EE.Simple.Content.of_email email in
    let mimetype = EE.Simple.Content.content_type content in
    match mimetype with
    | "text/plain" -> parse_text email
    | "text/html" -> parse_html email
    | "multipart/mixed" -> parse_multi email
    | "multipart/alternative" -> parse_multi_alt email
    | "multipart/related" -> parse_multi email
    | mt -> "<" ^ mt ^ " Attachment>" in
  textify (EE.of_string data)

module Make (G : Git_ops.S) = struct
  
  module Store = G.Store
  module Raw = Maildir.Make_raw(G)
  open G

  let deliver_mail store input =
    let data = In_channel.input_all input in
    let plain_data = make_plain data in
    let temp_file = Filename.temp_file "gitmaildir" "plain_deliver" in
    let oc = Out_channel.create temp_file in
    Out_channel.output_string oc plain_data;
    Out_channel.close oc;
    let ic = In_channel.create temp_file in
    let plain_ref = Git.Reference.of_path Git.Path.(v "refs/heads/plain") in
    let plain_commit = get_branch_commit store plain_ref in
    plain_commit >>== Raw.deliver_mail store ic
    >>== update_ref store plain_ref >|= (fun a ->
    In_channel.close ic;
    Sys.remove temp_file;
    a)

  let delete_mail store path =
    let plain_ref = Git.Reference.of_path Git.Path.(v "refs/heads/plain") in
    let plain_commit = get_branch_commit store plain_ref in
    plain_commit >>== Raw.delete_mail store path
    >>== update_ref store plain_ref

  let move_mail store path new_path =
    let plain_ref = Git.Reference.of_path Git.Path.(v "refs/heads/plain") in
    let plain_commit = get_branch_commit store plain_ref in
    plain_commit >>== Raw.move_mail store path new_path
    >>== update_ref store plain_ref

  let add_mail_time time store path input =
    let plain_ref = Git.Reference.of_path Git.Path.(v "refs/heads/plain") in
    let plain_commit = get_branch_commit store plain_ref in
    plain_commit >>== Raw.add_mail_time time store path input
    >>== update_ref store plain_ref

  let add_mail = add_mail_time (Unix.time ())

  let init_gitmaildir store =
    init_empty_blob store
end
