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
    spawn: position;
    spawn_dir: direction;
    fun_sizes: int list;
    map: map;
}

let split = Str.split (Str.regexp ",")

let parse_direction str =
    match int_of_string str with
    | 0 -> East
    | 1 -> South
    | 2 -> West
    | 3 -> North
    | _ -> failwith "unknown direction encoding"

let cell_of_char c =
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
    let title = input_line f in
    let width = int_of_string (input_line f) in
    let height = int_of_string (input_line f) in
    let spawn_column = int_of_string (input_line f) in
    let spawn_row = int_of_string (input_line f) in
    let spawn_dir = parse_direction (input_line f) in
    let allowed_cmds = int_of_string (input_line f) in
    let fun_sizes = split (input_line f)
        |> List.map (fun e -> int_of_string e) in
    let cells_str = input_line f in
    close_in f;

    let cells = ref [] in
    for l = 0 to (height - 1) do
        for c = 0 to (width - 1) do
            let i = l*width + c in
            let c = String.get cells_str i in
            cells := (cell_of_char c)::!cells
        done
    done;

    {
        title;
        spawn = (spawn_row, spawn_column);
        spawn_dir;
        fun_sizes;
        map = {
            width;
            height;
            cells = !cells;
        };
    }
