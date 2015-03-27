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
    let puzzle = Puzzle.parse "puzzles/p644.rzl" in
    let vm = Vm.init puzzle in
    let vm = Vm.(set_bytecode [|
        Move;
        JumpIfNot(Puzzle.Green, 3);
        Rotate Left;
        JumpIfNot (Puzzle.Red, 6);
        Rotate Right;
        TailCall 7;
        TailCall 0;
        Move;
        JumpIfNot(Puzzle.Green, 10);
        Rotate Right;
        TailCall 7;
    |] vm)
    in
    Display.init 600 600 32;
    run vm 500;
    Display.close ()
