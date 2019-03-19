(* prepare a usable module for testing (Unix filesystem that is) *)
module Store = Git.Store.Make(Digestif.SHA1)(Git_unix.Fs)(Git.Inflate)(Git.Deflate)
module Git_ops = Gitmaildir.Git_ops.Make(Store)
module Locking_unix = Gitmaildir_unix.Locking_unix
module Maildir = Gitmaildir.Maildir.Make_granular(Git_ops)(Locking_unix)

(* git_ops tests *)

let get_last () =
  Alcotest.(check int) "Check TEST" 1 1

let git_ops_set = [
  "TEST", `Quick, get_last;
]

(* maildir tests *)

let test_t () =
  Alcotest.(check int) "Check TEST" 1 1

let maildir_set = [
  "TEST", `Quick, test_t;
]

(* run *)

let () =
  Alcotest.run "Gitmaildir tests" [
    "git_ops", git_ops_set;
    "maildir", maildir_set;
]
