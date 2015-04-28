(* bytecode offset *)
type offset = int

type instruction =
    | Move
    | Rotate of Puzzle.rotation
    | Color of Puzzle.color
    | Call of offset
    | TailCall of offset
    | Return
    | Jump of offset
    | JumpIfNot of Puzzle.color * offset
    | Exit
val string_of_instruction: instruction -> string

type bytecode = instruction array
val print_bytecode: bytecode -> unit

type state = {
    offset   : offset;
    stars    : int;
    stack    : offset list;
    map      : Puzzle.map;
    position : Puzzle.position;
    direction: Puzzle.direction;
    bytecode : bytecode;
    moved    : bool;
}

val init: Puzzle.t -> state
val copy: state -> state
val set_bytecode: bytecode -> state -> state

val step: state -> state

val is_solved: state -> bool
val is_out_of_map: state -> bool
val is_out_of_instruction: state -> bool

(* draw p cell_size s *)
val draw: Display.position -> int -> state -> unit
(* may_animate draw_env p cell_size s *)
val may_animate: (unit -> unit) -> Display.position -> int -> state -> unit
