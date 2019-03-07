val sync : Git.Store.t -> Fpath.t -> (unit * error) Lwt_result.t

val run_daemon : Git.Store.t -> Fpath.t -> unit
