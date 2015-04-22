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
    | Return
    | Jump of label
    | JumpIfNot of Puzzle.color * label

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
            let precs = [Label label] |> List.append prec in
            (JumpIfNot (color, label))::precs
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
    let precompile prog =
        let Program defs = prog in
        List.fold_left
            (fun acc d -> (List.rev (rev_precompile_definition d))::acc)
            []
            defs
        |> List.rev
    in
    let precompiled_defs = precompile prog in
    let offset = ref 0 in
    let labels =
        List.fold_left
        (fun labels p_d ->
            List.fold_left
            (fun labels pre_instr ->
                match pre_instr with
                | Label l -> LabelMap.add l !offset labels
                | _ -> incr offset; labels
            )
            labels
            p_d
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
