module Lf = Core.Lock_file

type t = string

let v path = path

let lock l = Lf.blocking_create l

let try_lock l = Lf.create l

let unlock l = Unix.remove l
