let () =
    let puzzle = Puzzle.parse "puzzles/p644.rzl" in
    let vm = Vm.init puzzle in
    Display.init 600 600 32;
    Vm.draw 0 0 32 vm 0 0;
    Display.delay 10_000;
    Display.close ()
