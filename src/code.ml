type symbol = string

type instruction =
    | Move
    | Rotate of Puzzle.rotation
    | Color of Puzzle.color
    | If of Puzzle.color * instruction
    | Call of symbol

type definition = Definition of symbol * instruction list

type program = Program of definition list

type label = string

module LabelMap = Map.Make (struct type t = label let compare = compare end)

type precompiled_instruction =
    | Label of label
    | Move
    | Rotate of Puzzle.rotation
    | Color of Puzzle.color
    | Call of symbol
    | TailCall of symbol
    | Return
    | Jump of label
    | JumpIfNot of Puzzle.color * label

let rec find_forward label pre_instrs =
    match pre_instrs with
    | [] -> None
    | (Label label')::t -> if label = label' then Some t
                                             else None
    | h::t -> find_forward label t

let compile prog =
    let instr_count = ref 0 in
    let gen_label =
        let next = ref 0 in
        let prefix = "__" in
        (fun () ->
            let label = prefix ^ (string_of_int !next) in
            incr next;
            label
        )
    in
    let rec precompile_instruction (instr: instruction): precompiled_instruction list =
        incr instr_count;
        match instr with
        | Move -> [Move]
        | Rotate r -> [Rotate r]
        | Color c -> [Color c]
        | If (color, instr) ->
            let label = gen_label () in
            let prec = precompile_instruction instr in
            (JumpIfNot (color, label))::(prec @ [Label label])
        | Call symbol -> [Call symbol]
    in
    let rev_precompile_definition d =
        let Definition (symbol, instrs) = d in
        incr instr_count;
        let res =
            List.fold_left
            (fun acc i -> List.rev_append (precompile_instruction i) acc)
            [Label symbol]
            instrs
        in
        Return::res
    in
    let rev_tail_call_optimization pre_def =
        let known_color = ref None in
        let is_possible color =
            match !known_color with
            | None -> true
            | Some c -> color = c
        in
        let rec is_tail_call l =
            let try_follow label l =
                match find_forward label l with
                | Some t -> is_tail_call t
                | None -> false (* We don't handle this case *)
            in
            match l with
            | [] -> failwith "unreachable"
            | h::t -> (
                match h with
                | Return -> true
                | Label _ -> is_tail_call t
                | Jump label -> try_follow label t
                | JumpIfNot (color, label) ->
                    if is_possible color then is_tail_call t
                    else try_follow label t
                | _ -> false
            )
        in
        let color_end_label = ref None in
        let rec iter l acc =
            match l with
            | [] -> acc
            | (Call symbol)::t ->
                let acc =
                    if is_tail_call t then (TailCall symbol)::acc
                                      else     (Call symbol)::acc
                in
                iter t acc
            | h::t -> (
                match h with
                | JumpIfNot (color, label) -> (
                    known_color := Some color;
                    color_end_label := Some label;
                )
                | Label label -> (
                    match !color_end_label with
                    | Some c_e_l ->
                        if label = c_e_l then (
                            known_color := None;
                            color_end_label := None;
                        )
                    | None -> ()
                )
                | _ -> ();
                ;
                iter t (h::acc)
            )
        in
        iter pre_def []
    in
    let precompile prog =
        let Program defs = prog in
        List.fold_left
            (fun acc d -> (d
                |> rev_precompile_definition |> List.rev
                |> rev_tail_call_optimization |> List.rev)::acc
            )
            []
            defs
        |> List.rev
    in
    let precompiled_defs = precompile prog in
    let offset = ref 0 in
    let labels =
        List.fold_left
        (fun labels pre_def ->
            List.fold_left
            (fun labels pre_instr ->
                match pre_instr with
                | Label l -> LabelMap.add l !offset labels
                | _ -> incr offset; labels
            )
            labels
            pre_def
        )
        LabelMap.empty
        precompiled_defs
    in
    let compile_instruction (instr: precompiled_instruction): Vm.instruction =
        let open Vm in
        match instr with
        | Move -> Move
        | Rotate r -> Rotate r
        | Color c -> Color c
        | Call symbol -> Call (LabelMap.find symbol labels)
        | TailCall symbol -> TailCall (LabelMap.find symbol labels)
        | Return -> Return
        | Jump label -> Jump (LabelMap.find label labels)
        | JumpIfNot (color, label) -> JumpIfNot (color, LabelMap.find label labels)
        | Label _ -> failwith "unreachable"
    in
    let bytecode = Array.make (!instr_count + 1) Vm.Exit in
    let offset = ref 0 in
    let compile_definition =
        List.iter
        (fun pre_instr ->
            match pre_instr with
            | Label l -> ()
            | _ ->
                bytecode.(!offset) <- compile_instruction pre_instr;
                incr offset;
        )
    in
    List.iter compile_definition precompiled_defs;
    bytecode
