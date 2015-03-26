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

(* parse file_path *)
val parse: string -> t

val string_of_position: position -> string
val string_of_direction: direction -> string
val string_of_color: color -> string
val string_of_cell: cell -> string
