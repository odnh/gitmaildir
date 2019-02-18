module Unix = Core.Unix

type t = Unix.File_descr.t * Mutex.t

let v path = (Unix.openfile ~mode:[O_WRONLY; O_CREAT] path, Mutex.create ())

let lock l = Flock.flock (fst l) Flock.LOCK_EX; Mutex.lock (snd l)

let unlock l = Flock.flock (fst l) Flock.LOCK_UN; Mutex.unlock (snd l)
