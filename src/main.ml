let rec run vm timeout =
    if timeout = 0 then ()
    else (
    Vm.draw 0 0 32 vm 0 0;
    if Vm.is_solved vm then (
        Display.draw_text (200, 200) "Success";
        Display.sync ();
        Display.delay 2_000
    )
    else if Vm.is_out_of_instruction vm then ()
    else if Vm.is_out_of_map vm then (
        Display.draw_text (200, 200) "Failure";
        Display.sync ();
        Display.delay 2_000
    )
    else (
        Display.delay 50;
        run (Vm.step vm) (timeout - 1)
    )
    )

let () =
    let open Code in
    let open Vm in
    let open Puzzle in
    let puzzle = parse "puzzles/p644.rzl" in
    let prog = Program [
        Definition ("f1", [
            Move;
            If (Green, Rotate Left);
            If (Red, Rotate Right);
            If (Red, Call "f2");
            Call "f1";
        ]);
        Definition ("f2", [
            Move;
            If (Green, Rotate Right);
            Call "f2";
        ])
    ] in
    let bytecode = compile prog in
    let vm = init puzzle |> set_bytecode bytecode in
    Printf.printf "bytecode:\n";
    Array.iteri (fun i e -> Printf.printf "%d: %s\n" i (string_of_instruction e)) bytecode;
    Display.init 600 600 32;
    run vm 500;
    Display.close ()
