let () =
    let puzzle = Puzzle.parse "puzzles/p644.rzl" in
    Graphics.init 600 600 16;
    Graphics.delay 1000;
    Graphics.quit ();
    ()
