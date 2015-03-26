module IntMap: sig
    type key = int
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> key) -> 'a t -> 'a t -> key
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> key
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
end

type rotation = Left | Right

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
    position : Puzzle.position;
    direction: Puzzle.direction;
    bytecode : offset instruction IntMap.t;
}

val init: Puzzle.t -> state
val set_bytecode: offset instruction IntMap.t -> state -> state
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

val string_of_rotation: rotation -> string
val string_of_instruction: ('a -> string) -> 'a instruction -> string
