type rotation = Left | Right
val string_of_rotation: rotation -> string

(* bytecode offset *)
type offset = int

type instruction =
    | Label of offset
    | Move
    | Rotate of rotation
    | Call of offset
    | TailCall of offset
    | Return
    | SetColor of Puzzle.color
    | Jump of offset
    | JumpIfNot of Puzzle.color * offset
    | Exit
val string_of_instruction: instruction -> string

type state = {
    offset   : offset;
    stars    : int;
    stack    : offset list;
    map      : Puzzle.map;
    position : Puzzle.position;
    direction: Puzzle.direction;
    bytecode : instruction array;
}

val init: Puzzle.t -> state
val set_bytecode: instruction array -> state -> state
val init_stack: int -> state -> state

val step: state -> state

val is_solved: state -> bool
val is_out_of_map: state -> bool
val is_out_of_instruction: state -> bool

val get_pos: state -> Puzzle.position
val get_map: state -> Puzzle.map
val get_dir: state -> Puzzle.direction

(* draw offx offy cell_size state anim_steps anim_frame *)
val draw : int -> int -> int -> state -> int -> int -> unit
