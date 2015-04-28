let () =
    let cell_size = 32 in
    let puzzle = Puzzle.parse "current_puzzle.rzl" in
    Puzzle.print_info puzzle;

    let editor = Editor.init puzzle in
    Editor.print_info ();

    flush stdout;

    let vm = Vm.init puzzle in

    let draw vm editor =
        let open Puzzle in
        Vm.draw (0, 0) cell_size vm;
        Editor.draw (cell_size, cell_size + cell_size*puzzle.map.height)
                    cell_size editor;
    in
    let may_animate vm editor =
        Vm.may_animate (fun () -> draw vm editor) (0, 0) cell_size vm;
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
            draw vm editor;
            Display.sync ();
            edit ()
        )
    and run () =
        let prog = Editor.get_code editor in
        let bytecode = Code.compile prog in
        print_endline "bytecode:";
        Vm.print_bytecode bytecode;
        let rec iter vm =
            let abort = ref false in
            Events.handle () ~on_key_pressed:(fun k ->
                let open Sdlkey in
                match k with
                | KEY_ESCAPE -> abort := true
                | _ -> ()
            );
            if not (!abort || Events.should_quit ()) then (
                Display.clear ();
                draw vm editor;
                Printf.printf "-%d" vm.Vm.offset;
                flush stdout;
                Display.sync ();
                may_animate vm editor;
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
                    iter (Vm.step vm)
                );
            );
        in
        iter (Vm.set_bytecode bytecode (Vm.copy vm));
        print_endline ""
    in

    Display.init 600 600 32;
    edit ();
    Display.close ()
