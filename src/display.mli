type position = int * int

(* init window_width window_height cell_size *)
val init: int -> int -> int -> unit
val close: unit -> unit

(* clears the render buffer *)
val clear: unit -> unit
(* swaps the render buffer with the displayed buffer *)
val sync: unit -> unit

val draw_cell: position -> Puzzle.color option -> unit
val draw_cursor: position -> unit
val draw_star: position -> unit
(* draw_robot p d anim_frame *)
val draw_robot: position -> Puzzle.direction -> int -> unit
val draw_arrow: position -> Puzzle.direction -> unit
val draw_call: position -> string -> unit
val draw_text: position -> string -> unit

(* delay milliseconds *)
val delay: int -> unit
