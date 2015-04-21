type t

val init: Puzzle.t -> t
val update: t -> Sdlkey.t -> unit
val get_code: t -> Code.program

val print_info: unit -> unit

(* draw offset cell_size editor *)
val draw: Display.position -> int -> t -> unit
