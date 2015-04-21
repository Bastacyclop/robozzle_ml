open Code

type definition = instruction option array

type t = {
    definitions: definition array;
    mutable def: int;
    mutable instr: int;
}

let definition_count editor =
    Array.length editor.definitions

let instruction_count editor definition =
    Array.length editor.definitions.(definition)

let init puzzle =
    let open Puzzle in
    let rec count_funs l count =
        match l with
        | [] -> count
        | h::t ->
            if h > 0 then count_funs t (count + 1)
            else count
    in
    let fun_count = count_funs puzzle.fun_sizes 0 in
    let definitions = Array.make fun_count [||] in
    let rec init_definitions l d =
        if d < fun_count then (
            match l with
            | h::t ->
                definitions.(d) <- Array.make h None;
                init_definitions t (d + 1)
            | [] -> failwith "unreachable"
        )
    in
    init_definitions puzzle.fun_sizes 0;
    {
        definitions;
        def = 0;
        instr = 0;
    }

let set_cell editor cell =
    let definition = editor.definitions.(editor.def) in
    definition.(editor.instr) <- cell

let set_instr instr editor =
    set_cell editor (Some instr)

let pop_instr editor =
    set_cell editor None

let wait_color () =
    let open Puzzle in
    let open Sdlkey in
    Events.wait_key (fun k ->
        match k with
        | KEY_r -> Some Red
        | KEY_g -> Some Green
        | KEY_b -> Some Blue
        | _ -> None
    )

let wait_rotation () =
    let open Puzzle in
    let open Sdlkey in
    Events.wait_key (fun k ->
        match k with
        | KEY_LEFT -> Some Left
        | KEY_RIGHT -> Some Right
        | _ -> None
    )

let rec instruction_of_key k =
    let open Puzzle in
    let open Sdlkey in
    match k with
    | KEY_m -> Some Move
    | KEY_r ->
        let r = wait_rotation () in
        Some (Rotate r)
    | KEY_c ->
        let c = wait_color () in
        Some (Color c)
    | KEY_i ->
        let c = wait_color () in
        let i = wait_instruction () in
        Some (If (c, i))
    | KEY_F1 -> Some (Call "f1")
    | KEY_F2 -> Some (Call "f2")
    | KEY_F3 -> Some (Call "f3")
    | KEY_F4 -> Some (Call "f4")
    | KEY_F5 -> Some (Call "f5")
    | _ -> None
and wait_instruction () =
    Events.wait_key instruction_of_key

let update editor key =
    let open Puzzle in
    let open Sdlkey in
    let last_def = (definition_count editor) - 1 in
    let last_instr = (instruction_count editor editor.def) - 1 in
    let fit_instr () =
        let last_instr = (instruction_count editor editor.def) - 1 in
        editor.instr <- (min editor.instr last_instr)
    in
    match key with
    | KEY_d -> pop_instr editor
    | KEY_LEFT ->
        editor.instr <- if editor.instr = 0 then last_instr
                        else (editor.instr - 1)
    | KEY_RIGHT ->
        editor.instr <- if editor.instr = last_instr then 0
                        else (editor.instr + 1)
    | KEY_UP ->
        editor.def <- if editor.def = 0 then last_def
                      else (editor.def - 1);
        fit_instr ();
    | KEY_DOWN ->
        editor.def <- if editor.def = last_def then 0
                      else (editor.def + 1);
        fit_instr ();
    | _ ->
        match instruction_of_key key with
        | Some i -> set_instr i editor
        | None -> ()

let print_info () =
    print_string
"
+------------------- Controls -------------------+
| Left/Down/Right/Up: navigate through the code  |
| d: delete the current instruction              |
|------------------ instructions ----------------|
| m: move                                        |
| c: color (awaits a color)                      |
| i: if (awaits a color and an instruction)      |
| Fi: call 'fi'                                  |
|-------------------- colors --------------------|
| r: red                                         |
| g: green                                       |
| b: blue                                        |
+------------------------------------------------+
"

let get_code editor =
    let rec iter_def prog d =
        let d_index = d - 1 in
        let rec iter_instr def i =
            let i_index = i - 1 in
            if i = 0 then def
            else
                let def =
                    match editor.definitions.(d_index).(i_index) with
                    | Some instr -> instr::def
                    | None -> def
                in
                iter_instr def (i - 1)
        in
        if d = 0 then prog
        else
            let def = Definition (("f" ^ (string_of_int d)),
                (iter_instr [] (instruction_count editor d_index))
            ) in
            iter_def (def::prog) (d - 1)
    in
    Program (iter_def [] (definition_count editor))

let draw_cell pos cell =
    let open Code in
    let open Puzzle in
    Display.draw_cell pos None;
    let rec draw_instruction i =
        match i with
        | Move -> Display.draw_arrow pos North
        | Rotate Left -> Display.draw_arrow pos West
        | Rotate Right -> Display.draw_arrow pos East
        | Color c -> Display.draw_cell pos (Some c)
        | If (color, instr) ->
            Display.draw_cell pos (Some color);
            draw_instruction instr
        | Call symbol -> Display.draw_call pos symbol
    in
    match cell with
    | Some instr -> draw_instruction instr
    | None -> ()

let draw (off_x, off_y) cell_size editor =
    let pos_x = ref off_x in
    let pos_y = ref off_y in

    let last_def = (definition_count editor) - 1 in
    for d = 0 to last_def do
        let def = editor.definitions.(d) in

        let last_instr = (instruction_count editor d) - 1 in
        for i = 0 to last_instr do
            let cell = def.(i) in

            draw_cell (!pos_x, !pos_y) cell;

            pos_x := !pos_x + cell_size;
        done;

        pos_x := off_x;
        pos_y := !pos_y + cell_size;
    done;

    Display.draw_cursor ((off_x + editor.instr*cell_size),
                         (off_y + editor.def*cell_size))
