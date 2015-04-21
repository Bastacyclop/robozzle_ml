let received_quit = ref false
let should_quit () = !received_quit

let received_expose = ref false
let should_expose () = !received_expose

let handle ?(on_key_pressed=(fun _ -> ())) () =
    received_quit := false;
    received_expose := false;
    let rec handle_event e =
        match e with
        | None -> ()
        | Some e ->
            let open Sdlevent in
            match e with
            | QUIT -> received_quit := true
            | VIDEOEXPOSE -> received_expose := true
            | KEYDOWN e -> on_key_pressed e.keysym
            | _ -> ();
            handle_event (Sdlevent.poll ())
    in
    handle_event (Sdlevent.poll ())

let rec wait f =
    let e = Sdlevent.wait_event () in
    let x = f e in
    match x with
    | Some x -> x
    | None -> wait f

let wait_key f =
    wait (fun e ->
        match e with
        | Sdlevent.KEYDOWN e -> f (e.Sdlevent.keysym)
        | _ -> None
    )
