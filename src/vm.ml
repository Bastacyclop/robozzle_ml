module IntMap = Map.Make(struct type t = int let compare = compare end)

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

let get_cell (l, c) (map: Puzzle.map): Puzzle.cell =
    let open Puzzle in
    let rec iter_c c cells =
        if c = 0 then cells
        else match cells with
        | [] -> failwith "cell out of bounds"
        | h::t -> iter_c (c - 1) t
    in
    let rec iter_l l cells =
        if l = 0 then cells
        else iter_l (l - 1) (iter_c map.width cells)
    in
    match iter_l l map.cells with
    | [] -> failwith "cell out of bounds"
    | h::t -> h

let count_stars (puzzle: Puzzle.t) =
    let open Puzzle in
    let rec iter cells acc =
        match cells with
        | [] -> acc
        | h::t ->
            let acc = if h.star then acc + 1 else acc in
            iter t acc
    in iter puzzle.map.cells 0

let init (puzzle: Puzzle.t) =
    let open Printf in
    let open Puzzle in
    printf "spawn: %s - %s\n" (string_of_position puzzle.spawn_pos)
                              (string_of_direction puzzle.spawn_dir);
    {
        offset = 0;
        stars = count_stars puzzle;
        stack = [];
        map = puzzle.map;
        position = puzzle.spawn_pos;
        direction = puzzle.spawn_dir;
        bytecode = IntMap.empty;
    }

let set_bytecode bytecode (state: state) =
    { state with bytecode; }

(* TODO *)
let init_stack id (state: state) = state

(* TODO *)
let step (state: state) = state

let is_solved (state: state) =
    state.stars = 0

let is_out_of_map (state: state) =
    let open Puzzle in
    let m = state.map in
    let (l, c) = state.position in
    l < 0 || m.height <= l ||
    c < 0 || m.width  <= c || (
        let c = get_cell (l, c) m in
        match c.color with
        | None -> true
        | _ -> false
    )

let is_out_of_instruction (state: state) =
    let len = IntMap.cardinal state.bytecode in
    state.offset >= len

let get_pos (state: state) = state.position
let get_map (state: state) = state.map
let get_dir (state: state) = state.direction

let draw offx offy cell_size (state: state) anim_steps anim_frame =
    let open Puzzle in
    let draw_cells () =
        let pos = ref (offx, offy) in
        let rec iter_c i (cells: cell list) =
            if i = 0 then cells
            else match cells with
            | [] -> failwith "index out of bounds"
            | h::t ->
                Display.draw_cell !pos h.color;
                if h.star then Display.draw_star !pos;
                let (x, y) = !pos in pos := ((x + cell_size), y);
                iter_c (i - 1) t
        in
        let rec iter_l i cells =
            if i = 0 then ()
            else
                let cells = iter_c state.map.width cells in
                let (x, y) = !pos in pos := (0, (y + cell_size));
                iter_l (i - 1) cells
        in iter_l state.map.height state.map.cells
    in
    let draw_robot () =
        let (rl, rc) = state.position in
        let rx = offx + rc*cell_size in
        let ry = offy + rl*cell_size in
        Display.draw_robot (rx, ry) state.direction 0
    in
    draw_cells ();
    draw_robot ();
    Display.sync ()

let string_of_rotation rot =
    match rot with
    | Left -> "Left"
    | Right -> "Right"

let string_of_instruction f instr =
    match instr with
    | Label x -> "Label " ^ (f x)
    | Move -> "Move"
    | Rotate rot -> "Rotate " ^ (string_of_rotation rot)
    | Call x -> "Call " ^ (f x)
    | TailCall x -> "TailCall " ^ (f x)
    | Return -> "Return"
    | SetColor c -> "SetColor " ^ (Puzzle.string_of_color c)
    | Jump x -> "Jump " ^ (f x)
    | JumpIfNot (c, x) -> "JumpIfNot " ^ (Puzzle.string_of_color c) ^ " " ^ (f x)
    | Exit -> "Exit"

