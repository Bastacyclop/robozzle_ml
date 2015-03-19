type position = int * int

(* init window_width window_height cellule_size *)
val init : int -> int -> int -> unit

val quit : unit -> unit

val clear : unit -> unit

val draw_cell: pos -> Puzzle.color -> unit
val draw_cursor: pos -> unit
val draw_star: pos -> unit
(* draw_robot p d anim_frame *)
val draw_robot: pos -> Puzzle.direction -> int -> unit
val draw_arrow: pos -> Puzzle.direction -> unit
val draw_call: pos -> string -> unit
val draw_text: pos -> string -> unit

val sync: unit -> unit

(* delay milliseconds *)
val delay: int -> unit
