type position = int * int

type direction =
    | East
    | North
    | West
    | South

type color =
    | Red
    | Green
    | Blue

type cell = {
    color: color option;
    star: bool;
}

type map = {
    width: int;
    height: int;
    cells: cell list;
}

type t = {
    title: string;
    spawn_pos: position;
    spawn_dir: direction;
    fun_sizes: int list;
    map: map;
}

let split = Str.split (Str.regexp ",")

let decode_direction str =
    match int_of_string str with
    | 0 -> East
    | 1 -> South
    | 2 -> West
    | 3 -> North
    | _ -> failwith "unknown direction encoding"

let decode_cell c =
    match c with
    | '.' -> { color = None; star = false; }
    | 'r' -> { color = Some Red; star = false; }
    | 'R' -> { color = Some Red; star = true; }
    | 'g' -> { color = Some Green; star = false; }
    | 'G' -> { color = Some Green; star = true; }
    | 'b' -> { color = Some Blue; star = false; }
    | 'B' -> { color = Some Blue; star = true; }
    | _ -> failwith "unknown cell encoding"

let parse file_path =
    let f = open_in file_path in
    let read_number () = int_of_string (input_line f) in
    let title = input_line f in
    let width = read_number () in
    let height = read_number () in
    let spawn_column = read_number () in
    let spawn_line = read_number () in
    let spawn_dir = decode_direction (input_line f) in
    let allowed_cmds = read_number () in
    let fun_sizes = split (input_line f)
        |> List.map (fun e -> int_of_string e) in
    let cells_str = input_line f in
    close_in f;

    let cells = ref [] in
    for l = (height - 1) downto 0 do
        for c = (width - 1) downto 0 do
            let i = l*width + c in
            let c = String.get cells_str i in
            cells := (decode_cell c)::!cells
        done
    done;
    let cells = !cells in

    {
        title;
        spawn_pos = (spawn_line, spawn_column);
        spawn_dir;
        fun_sizes;
        map = {
            width;
            height;
            cells;
        };
    }

let string_of_position (x, y) =
    "(" ^ (string_of_int x) ^ ", " ^ (string_of_int y) ^ ")"

let string_of_direction dir =
    match dir with
    | East -> "East"
    | North -> "North"
    | West -> "West"
    | South -> "South"

let string_of_color c =
    match c with
    | Red -> "Red"
    | Green -> "Green"
    | Blue -> "Blue"

let string_of_cell cell =
    match cell.color with
    | None -> "Empty"
    | Some c -> string_of_color c
    ^ if cell.star then "*" else ""
    ^ " Cell"
