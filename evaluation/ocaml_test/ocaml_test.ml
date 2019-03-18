open Core
open Lwt.Infix

module Md = Maildir
module Mb = Mbox

module Store = Git.Store.Make(Digestif.SHA1)(Git_unix.Fs)(Git.Inflate)(Git.Deflate)
module Git_ops = Gitmaildir.Git_ops.Make(Store)
(*module Locking_unix = Gitmaildir_unix.Locking_unix
module Maildir = Gitmaildir.Maildir.Make_granular(Git_ops)(Locking_unix)*)
module Maildir = Gitmaildir.Maildir.Make_unsafe(Git_ops)

let store_of_string path =
  Store.v () Fpath.(v path)
  >|= function
    | Ok s -> s
    | Error _ -> failwith "Bad Store"

let err_out err =
  Git_ops.pp_error Format.std_formatter err;
  Out_channel.newline stdout

let gmd_deliver store filename =
  let input = In_channel.create filename in
  let store = store_of_string store in
  let deliver_lwt = store >>= (fun s -> Maildir.deliver_mail s input) in
  match Lwt_main.run deliver_lwt with
  | Ok _ -> ()
  | Error e -> err_out e

let md_deliver store filename =
  let input = In_channel.create filename in
  Md.deliver input store
  
let mb_deliver store filename =
  let input = In_channel.create filename in
  Mb.deliver input store

let sequential_test ~f ~store ~data ~log =
  let test_files = Sys.readdir data in
  let t_start = Unix.gettimeofday () in
  Array.iter test_files ~f:(fun tf -> f store (data ^ "/" ^ tf));
  let t_end = Unix.gettimeofday () in
  let total_time = string_of_float @@ t_end -. t_start in
  let log_file = Out_channel.create ~append:true log in
  Out_channel.output_string log_file total_time

let parallel_test ~f ~store ~data ~log =
  let rec parallel_exe = function
    | [] -> ()
    | x::xs ->
        (match Unix.fork () with
        | `In_the_child -> f store (data ^ "/" ^ x); Unix.exit_immediately 0
        | `In_the_parent pid -> parallel_exe xs; Unix.waitpid_exn pid) in
  let test_files = Sys.readdir data in
  let t_start = Unix.gettimeofday () in
  parallel_exe (Array.to_list test_files);
  let t_end = Unix.gettimeofday () in
  let total_time = string_of_float @@ t_end -. t_start in
  let log_file = Out_channel.create ~append:true log in
  Out_channel.output_string log_file total_time

let parallel_n_test ~f ~store ~data ~log ~threads =
  let mutex = Mutex.create () in
  let condition = Condition.create () in
  let counter = ref threads in
  let enter () =
    Mutex.lock mutex;
    if !counter > 0 then
      counter := !counter - 1
    else
      Condition.wait condition mutex
  in
  let exit () =
    counter := !counter + 1;
    Condition.signal condition;
    Mutex.unlock mutex in
  let rec parallel_exe = function
    | [] -> ()
    | x::xs ->
        (match enter (); Unix.fork () with
        | `In_the_child -> f store (data ^ "/" ^ x); exit (); Unix.exit_immediately 0
        | `In_the_parent pid -> parallel_exe xs; Unix.waitpid_exn pid) in
  let test_files = Sys.readdir data in
  let t_start = Unix.gettimeofday () in
  parallel_exe (Array.to_list test_files);
  let t_end = Unix.gettimeofday () in
  let total_time = string_of_float @@ t_end -. t_start in
  let log_file = Out_channel.create ~append:true log in
  Out_channel.output_string log_file total_time

(* Command-line interface *)

let chosen_test store data log store_type test =
  let backend = match store_type with
                | "gmd" -> gmd_deliver
                | "md" -> md_deliver
                | "mb" -> mb_deliver
                | _ -> failwith "Not valid backend" in
  let test_type = match test with
                  | "tdp" -> parallel_test
                  | "tds" -> sequential_test
                  | "tdn" -> parallel_n_test ~threads:10
                  | _ -> failwith "Not valid test type" in
  test_type ~f:backend ~store ~data ~log

open Cmdliner

let store_arg =
  let doc = "Path of store to use" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"STORE_PATH" ~doc)

let data_arg =
  let doc = "Path of data to use" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"DATA_PATH" ~doc)

let log_arg =
  let doc = "Path of file to write results to" in
  Arg.(required & pos 2 (some string) None & info [] ~docv:"LOG_PATH" ~doc)

let type_arg =
  let doc = "The type of storage backend to use" in
  Arg.(required & pos 3 (some string) None & info [] ~docv:"STORE_TYPE" ~doc)

let test_arg =
  let doc = "The test to run" in
  Arg.(required & pos 4 (some string) None & info [] ~docv:"TEST_NAME" ~doc)

let execute_info =
  let doc = "Execute the test" in
  Term.info "execute" ~doc ~exits:Term.default_exits

let execute_t = Term.(const chosen_test $ store_arg $ data_arg $ log_arg $ type_arg $ test_arg)

let () = Term.exit @@ Term.eval (execute_t, execute_info)
