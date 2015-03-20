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

(* parse file_path *)
val parse: string -> t
