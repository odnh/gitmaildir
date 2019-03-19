open Core
open Gitmaildir.Lwt_result_helpers

module Store = Git.Store.Make(Digestif.SHA1)(Git_unix.Fs)(Git.Inflate)(Git.Deflate)
module Git_ops = Gitmaildir.Git_ops.Make(Store)
module Lock = Gitmaildir_unix.Locking_unix
module Maildir = Gitmaildir.Maildir.Make_granular(Git_ops)(Locking_unix)

(*
let lock = Lock.v ".global_lock"

let diff_trees store dir =


let add_to_gitmaildir_fold errors file =
  errors >>== fun (() ->
  In_channel.create file |> fun (input ->
  Maildir.add_mail store file input |> fun (result ->
  In_channel.close input;
  result (* May have problem of closing input before it's actually used *)

let sync store dir =
  Lock.lock lock;
  let new_git, new_maildir = diff_trees store dir in
  List.fold new_maildir errors ~f:add_to_gitmaildir_fold
  (* now do the same in other direction *)
  Lock.unlock lock

let run_daemon store dir = ()
  (* run synchronisation *)
  (* then enter filesystem watching *)
  sync store dir;
*)
