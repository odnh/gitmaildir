(** lwt_option_bind_both but for a function taking 2 values *)
let lwt_result_bind2 f a b =
  Lwt.bind a (function
    | Ok a -> Lwt.bind b (function
      | Ok b -> f a b
      | Error e -> Lwt.return (Error e))
    | Error e -> Lwt.return (Error e))

let (>>||) x f = Lwt_result.map f x

let (>>=|) = Lwt_result.bind_result

let (>>==) = Lwt_result.bind

let option_pair = function
  | Some a, Some b -> Some (a, b)
  | _ -> None
