let () =
    let cell_size = 32 in
    let puzzle = Puzzle.parse "puzzles/p644.rzl" in
    Puzzle.print_info puzzle;

    let editor = Editor.init puzzle in
    Editor.print_info ();
    let draw_editor editor =
        let open Puzzle in
        Editor.draw (50, 50 + cell_size*puzzle.map.height) cell_size editor;
    in

    flush stdout;

    let vm = Vm.init puzzle in
    let draw_vm vm =
        Vm.draw 0 0 cell_size vm 0 0;
    in

    let rec edit () =
        Events.handle () ~on_key_pressed:(fun k ->
            let open Sdlkey in
            match k with
            | KEY_RETURN -> run ()
            | _ -> Editor.update editor k
        );
        if not (Events.should_quit ()) then (
            Display.clear ();
            draw_vm vm;
            draw_editor editor;
            Display.sync ();
            edit ()
        )
    and run () =
        let prog = Editor.get_code editor in
        let bytecode = Code.compile prog in
        Printf.printf "bytecode:\n";
        Vm.print_bytecode bytecode;
        let rec iter vm =
            Events.handle ();
            if not (Events.should_quit ()) then (
                Display.clear ();
                draw_vm vm;
                Printf.printf "-%d" vm.Vm.offset;
                if Vm.is_solved vm then (
                    Display.draw_text (200, 200) "Success";
                    Display.sync ();
                    Events.wait_key (fun _ -> Some ())
                ) else if Vm.is_out_of_instruction vm
                     || Vm.is_out_of_map vm then (
                    Display.draw_text (200, 200) "Failure";
                    Display.sync ();
                    Events.wait_key (fun _ -> Some ())
                ) else (
                    Display.sync ();
                    Display.delay 50;
                    iter (Vm.step vm)
                );
            );
            flush stdout
        in
        iter (Vm.set_bytecode bytecode vm)
    in
    Display.init 600 600 32;
    edit ();
    Display.close ()
