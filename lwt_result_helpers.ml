let lwt_option_map a b =
  Lwt.map (fun x -> match x with Some x -> Some (a x) | None -> None) b

let lwt_option_bind a b =
  Lwt.map (fun x -> match x with Some x -> a x | None -> None) b

let lwt_option_bind_both a b = Lwt.bind b (function | Some s -> a s | None -> Lwt.return_none)

let lwt_option_bind2 f a b =
  Lwt.bind a (function
    | Some a -> Lwt.bind b (function
      | Some b -> f a b
      | None -> Lwt.return_none)
    | None -> Lwt.return_none)

let (>>>|) a b = lwt_option_map b a

let (>>>=) a b = lwt_option_bind b a

let (>>==) a b = lwt_option_bind_both b a

let option_pair = function
  | Some a, Some b -> Some (a, b)
  | _ -> None
