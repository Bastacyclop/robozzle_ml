type position = int * int
val string_of_position: position -> string

type direction =
    | East
    | North
    | West
    | South
val string_of_direction: direction -> string

type color =
    | Red
    | Green
    | Blue
val string_of_color: color -> string

type cell = {
    color: color option;
    star: bool;
}
val empty_cell: cell
val string_of_cell: cell -> string


type map = {
    width: int;
    height: int;
    cells: cell array;
}

type t = {
    title: string;
    spawn_pos: position;
    spawn_dir: direction;
    fun_sizes: int list;
    map: map;
}

(* parse file_path *)
val parse: string -> t
