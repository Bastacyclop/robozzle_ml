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
    bytecode : bytecode
}

val init: Puzzle.t -> state
val copy: state -> state
val set_bytecode: bytecode -> state -> state

val step: state -> state

val is_solved: state -> bool
val is_out_of_map: state -> bool
val is_out_of_instruction: state -> bool

val get_pos: state -> Puzzle.position
val get_map: state -> Puzzle.map
val get_dir: state -> Puzzle.direction

(* draw pos cell_size state anim_steps anim_frame *)
val draw : Display.position -> int -> state -> int -> int -> unit
