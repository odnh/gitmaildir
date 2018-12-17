open Core
open Git_unix
(*open Lwt.Infix*)

(*type t*)

let store_of_string path =
  Fpath.v path
  |> Store.v

let add_to_store store input =
  In_channel.input_all input
  |> Store.Value.Blob.of_string 
  |> Store.Value.blob
  |> Store.write store


