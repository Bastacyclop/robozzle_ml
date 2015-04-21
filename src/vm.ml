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

let string_of_instruction instr =
    let open Puzzle in
    let f = string_of_int in
    match instr with
    | Move -> "Move"
    | Rotate rot -> "Rotate " ^ (string_of_rotation rot)
    | Color c -> "Color " ^ (string_of_color c)
    | Call x -> "Call " ^ (f x)
    | TailCall x -> "TailCall " ^ (f x)
    | Return -> "Return"
    | Jump x -> "Jump " ^ (f x)
    | JumpIfNot (c, x) -> "JumpIfNot " ^ (string_of_color c) ^ " " ^ (f x)
    | Exit -> "Exit"

type bytecode = instruction array

let print_bytecode =
    Array.iteri
        (fun i e -> Printf.printf "%d: %s\n" i (string_of_instruction e))

type state = {
    offset   : offset;
    stars    : int;
    stack    : offset list;
    map      : Puzzle.map;
    position : Puzzle.position;
    direction: Puzzle.direction;
    bytecode : bytecode;
}

let cell_index (l, c) m =
    Puzzle.(l*m.width + c)

let get_cell (l, c) (m: Puzzle.map): Puzzle.cell =
    Puzzle.(m.cells.(cell_index (l, c) m))

let set_cell v (l, c) (m: Puzzle.map) =
    Puzzle.(m.cells.(cell_index (l, c) m) <- v)

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
    let open Puzzle in
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
    { state with bytecode; stack = [(Array.length bytecode) - 1] }

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

let may_collect_star position (state: state) =
    let open Puzzle in
    let cell = get_cell position state.map in
    if cell.star then (
        set_cell { cell with star = false; } position state.map;
        state.stars - 1
    ) else state.stars

let step (state: state) =
    let open Puzzle in
    let instr = state.bytecode.(state.offset) in
    match instr with
    | Move ->
        let position = move state.direction state.position in
        let stars = may_collect_star position state in
        { state with position; stars; offset = state.offset + 1; }
    | Rotate rot -> { state with
        direction = rotate rot state.direction;
        offset = state.offset + 1;
    }
    | Color color ->
        (* if there was a star we already collected it *)
        set_cell { color = Some color; star = false; } state.position state.map;
        { state with offset = state.offset + 1 }
    | Call offset -> { state with offset; stack = state.offset::state.stack; }
    | TailCall offset -> { state with offset; }
    | Return -> (
        match state.stack with
        | [] -> failwith "empty stack on return"
        | offset::stack -> { state with offset; stack; }
    )
    | Jump offset -> { state with offset; }
    | JumpIfNot (color, offset) ->
        let cell = get_cell state.position state.map in
        let offset = if cell.color <> Some color then offset
                     else state.offset + 1
        in { state with offset; }
    | Exit -> state


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
    state.bytecode.(state.offset) = Exit

let get_pos (state: state) = state.position
let get_map (state: state) = state.map
let get_dir (state: state) = state.direction

let draw offx offy cell_size (state: state) anim_steps anim_frame =
    let open Puzzle in
    let draw_cells () =
        let pos = ref (offx, offy) in
        for l = 0 to state.map.height - 1 do
            for c = 0 to state.map.width - 1 do
                let e = get_cell (l, c) state.map in
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
        Display.draw_robot (rx, ry) state.direction anim_frame
    in
    draw_cells ();
    draw_robot ()
