type rotation = Left | Right

let string_of_rotation rot =
    match rot with
    | Left  -> "Left"
    | Right -> "Right"

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

let string_of_instruction instr =
    let f = string_of_int in
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

type state = {
    offset   : offset;
    stars    : int;
    stack    : offset list;
    map      : Puzzle.map;
    position : Puzzle.position;
    direction: Puzzle.direction;
    bytecode : instruction array;
}

let get_cell (l, c) (m: Puzzle.map): Puzzle.cell ref =
    ref Puzzle.(m.cells.(l*m.width + c))

let count_stars (puzzle: Puzzle.t) =
    let open Puzzle in
    let map = puzzle.map in
    let acc = ref 0 in
    for i = 0 to (map.width*map.height) - 1 do
        let e = map.cells.(i) in
        if e.star then acc := !acc + 1
    done;
    !acc

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
        bytecode = [||];
    }

let set_bytecode bytecode (state: state) =
    { state with bytecode; }

(* TODO *)
let init_stack id (state: state) = state

let move dir (l, c) =
    let open Puzzle in
    match dir with
    | East  -> (l, c + 1)
    | South -> (l + 1, c)
    | West  -> (l, c - 1)
    | North -> (l - 1, c)

let rotate rot dir =
    let open Puzzle in
    match rot with
    | Left  -> (
        match dir with
        | East  -> North
        | South -> East
        | West  -> South
        | North -> West
    )
    | Right -> (
        match dir with
        | East  -> South
        | South -> West
        | West  -> North
        | North -> East
    )

let step (state: state) =
    let open Puzzle in
    let instr = Array.get state.bytecode state.offset in
    Printf.printf "%d: %s\n" state.offset (string_of_instruction instr);
    match instr with
    | Move -> { state with
        position = move state.direction state.position;
        offset = state.offset + 1;
    }
    | Rotate rot -> { state with
        direction = rotate rot state.direction;
        offset = state.offset + 1;
    }
    | Call offset -> { state with offset; stack = state.offset::state.stack; }
    | TailCall offset -> { state with offset; }
    | Return -> (
        match state.stack with
        | [] -> failwith "empty stack on return"
        | offset::stack -> { state with offset; stack; }
    )
    | SetColor color ->
        let cell = get_cell state.position state.map in
        cell := { !cell with color = Some color; };
        { state with offset = state.offset + 1 }
    | Jump offset -> { state with offset; }
    | JumpIfNot (color, offset) ->
        let cell = !(get_cell state.position state.map) in
        let offset = if cell.color <> Some color then offset
                     else state.offset + 1
        in { state with offset; }
    | Exit -> failwith "exit"
    | Label _ -> failwith "label"


let is_solved (state: state) =
    state.stars = 0

let is_out_of_map (state: state) =
    let open Puzzle in
    let m = state.map in
    let (l, c) = state.position in
    l < 0 || m.height <= l ||
    c < 0 || m.width  <= c || (
        let c = !(get_cell (l, c) m) in
        match c.color with
        | None -> true
        | _ -> false
    )

let is_out_of_instruction (state: state) =
    let len = Array.length state.bytecode in
    state.offset >= len

let get_pos (state: state) = state.position
let get_map (state: state) = state.map
let get_dir (state: state) = state.direction

let draw offx offy cell_size (state: state) anim_steps anim_frame =
    let open Puzzle in
    let draw_cells () =
        let pos = ref (offx, offy) in
        for l = 0 to state.map.height - 1 do
            for c = 0 to state.map.width - 1 do
                let e = !(get_cell (l, c) state.map) in
                Display.draw_cell !pos e.color;
                if e.star then Display.draw_star !pos;
                let (x, y) = !pos in pos := ((x + cell_size), y);
            done;
            let (x, y) = !pos in pos := (0, (y + cell_size));
        done;
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
