open Core
open Lwt.Infix
open Lwt_result_helpers

module type Locking = sig
  type t

  val v : string -> t

  val lock : t -> unit

  val unlock : t -> unit
end

module type Makeable = functor (G : Git_ops.S) ->
sig
  include
    Maildir.S
    with module Store = G.Store
    and type error := G.error
end

module Make_unsafe (M : Makeable) (P : Makeable) (G : Git_ops.S) = struct
  (* NB: was only able to implement this using the not granular locking *)

  module Store = G.Store

  (* Here we make our modules with the same Git_ops so that the types match NB: use non-locking *)
  module M = M(G)
  module P = P(G)

  let deliver_mail store input =
    let temp_file = Filename.temp_file "gitmaildir" "deliver" in
    let oc = Out_channel.create temp_file in
    Out_channel.output_string oc (In_channel.input_all input);
    Out_channel.close oc;
    let ic1 = In_channel.create temp_file in
    let ic2 = In_channel.create temp_file in
    M.deliver_mail store ic1 >>== (fun () ->
    P.deliver_mail store ic2) >|= (fun a ->
    In_channel.close ic1;
    In_channel.close ic2;
    Sys.remove temp_file;
    a)

  let delete_mail store path =
    M.delete_mail store path >>== fun () ->
    P.delete_mail store path

  let move_mail store path new_path =
    M.move_mail store path new_path >>== fun () ->
    P.move_mail store path new_path

  let add_mail_time time store path input =
    M.add_mail_time time store path input >>== fun () ->
    P.add_mail_time time store path input

  let add_mail = add_mail_time (Unix.time ())

  let init_gitmaildir store =
    M.init_gitmaildir store >>== fun () ->
    P.init_gitmaildir store
end

module Make_locking (M : Makeable) (P : Makeable) (L : Locking) (G : Git_ops.S) = struct

  module Store = G.Store
  module Unsafe = Make_unsafe(M)(P)(G)
  module Lock = L
  open G

  let get_lock store = Lock.v ((Fpath.to_string @@ Store.root store) ^ "/.global_lock")

  let deliver_mail store input =
    let lock = get_lock store in
    Lwt.return_unit >|= (fun () ->
    Lock.lock lock) >>= (fun () ->
    Unsafe.deliver_mail store input) >|= (fun a ->
    Lock.unlock lock; a)

  let delete_mail store path =
    let lock = get_lock store in
    Lwt.return_unit >|= (fun () ->
    Lock.lock lock) >>= (fun () ->
    Unsafe.delete_mail store path) >|= (fun a ->
    Lock.unlock lock; a)

  let move_mail store path new_path =
    let lock = get_lock store in
    Lwt.return_unit >|= (fun () ->
    Lock.lock lock) >>= (fun () ->
    Unsafe.move_mail store path new_path) >|= (fun a ->
    Lock.unlock lock; a)

  let add_mail_time time store path input =
    let lock = get_lock store in
    Lwt.return_unit >|= (fun () ->
    Lock.lock lock) >>= (fun () ->
    Unsafe.add_mail_time time store path input) >|= (fun a ->
    Lock.unlock lock; a)

  let add_mail = add_mail_time (Unix.time ())

  let init_gitmaildir store =
    init_empty_blob store
end

let add_plugins maildir_base plugin_list locking =
  let rec build acc = function
    | [] -> acc
    | x::[] -> (module Make_locking(val acc : Makeable)
                                   (val x : Makeable)
                                   (val locking : Locking) : Makeable)
    | x::xs -> build (module Make_unsafe(val acc : Makeable)
                                        (val x : Makeable) : Makeable) xs in
  build maildir_base plugin_list
