(** maps function over lwt and option *)
let lwt_result_map a b =
  Lwt.map (fun x -> match x with Ok x -> Ok (a x) | Error e -> Error e) b

(** maps function over lwt, binds over option *)
let lwt_result_bind a b =
  Lwt.map (fun x -> match x with Ok x -> a x | Error e -> Error e) b

(** binds function over both lwt and option *)
let lwt_result_bind_both a b =
  Lwt.bind b (function | Ok s -> a s | Error e -> Lwt.return (Error e))

(** lwt_option_bind_both but for a function taking 2 values *)
let lwt_result_bind2 f a b =
  Lwt.bind a (function
    | Ok a -> Lwt.bind b (function
      | Ok b -> f a b
      | Error e -> Lwt.return (Error e))
    | Error e -> Lwt.return (Error e))

let (>>>|) a b = lwt_result_map b a

let (>>>=) a b = lwt_result_bind b a

let (>>==) a b = lwt_result_bind_both b a

let option_pair = function
  | Some a, Some b -> Some (a, b)
  | _ -> None
