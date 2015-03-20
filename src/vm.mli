type rotation = Left | Right

type position = int * int

(* bytecode offset *)
type offset = int

type 'a instruction =
    | Label of 'a
    | Move
    | Rotate of rotation
    | Call of 'a
    | TailCall of 'a
    | Return
    | SetColor of Puzzle.color
    | Jump of 'a
    | JumpIfNot of Puzzle.color * 'a
    | Exit

type state = {
    offset   : offset;
    stars    : int;
    stack    : offset list;
    map      : Puzzle.map;
    position : position;
    direction: Puzzle.direction;
    bytecode : offset instruction IntMap.t;
}

val string_of_instruction: ('a -> string) -> 'a instruction -> string

val init: Puzzle.t -> state
val set_bytecode: state -> offset instruction IntMap.t -> state
val init_stack: state -> int -> state

val step: state -> state

val is_solved: state -> bool
val is_out_of_map: state -> bool
val is_out_of_instruction: state -> bool

val get_pos: state -> pos
val get_map: state -> Puzzle.map
val get_dir: state -> Puzzle.direction

(* draw offx offy cell_size state anim_steps anim_frame *)
val draw : int -> int -> int -> state -> int -> int -> unit
