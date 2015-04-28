type seconds = float
type t = seconds

val get: unit -> t

val wait: t -> unit

val do_n_every: (int -> unit) -> int -> t -> unit
