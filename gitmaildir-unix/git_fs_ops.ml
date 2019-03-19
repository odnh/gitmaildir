open Core
open Lwt.Infix

module Store = Git_unix.Store

let checkout_to_dir store commit path =
  Lwt.return_unit >>= fun () ->
  Lwt.return @@ Sys.command ("mkdir " ^ Fpath.to_string path) >>= fun _ ->
  Lwt.return @@ Sys.command ("git --git-dir=" ^ (Fpath.to_string (Store.dotgit store))
    ^ " --work-tree=" ^ (Fpath.to_string (Fpath.to_dir_path path))
    ^ " checkout " ^ (Store.Hash.to_hex commit) ^ " -- .") >>= fun _ ->
  Lwt.return_unit

(*
let convert_maildir maildir_impl store path =
  let module Maildir = (val maildir_impl) in
  let rec get_all_files result = function
    | f::fs when Sys.is_directory f = `Yes ->
        Sys.readdir f
        |> Array.to_list
        |> List.map ~f:(Filename.concat f)
        |> List.append fs
        |> get_all_files result
    | f::fs -> get_all_files (f::result) fs
    | [] -> result in
  let maildir_root_length = Fpath.segs path |> List.length in
  let all_files = get_all_files [] [Fpath.to_string path] in
  let sorted_files = all_files
    |> List.map ~f:(fun f -> f, (Unix.stat f).st_mtime)
    |> List.sort ~compare:(fun (_,t1) (_,t2) -> Float.compare t1 t2)
    |> List.map ~f:(fun (f,t) ->
        (Fpath.(v f |> segs)
          |> (fun l -> List.drop l maildir_root_length)
          |> String.concat ~sep:Fpath.dir_sep
        , f, t)) in
  List.iter sorted_files ~f:(fun (f1, f2, t) ->
    let input = In_channel.create f2 in
    let deliver = Maildir.add_mail_time t store (Fpath.v f1) input in
    let result = Lwt_main.run deliver in
    In_channel.close input;
    match result with
    | Ok () -> ()
    | Error e -> err_out e; failwith "Conversion Error")
*)
