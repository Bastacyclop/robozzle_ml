type seconds = float
type t = seconds

let get = Sys.time

let wait time =
    let mark = get () in
    let rec iter () =
        let d = get () -. mark in
        if d >= time then ()
        else iter ()
    in iter ()

let do_n_every f n time =
    (* we lose precision at each `wait`, that could be avoided *)
    assert (n > 0);
    f 1;
    for i = 2 to n do
        wait time;
        f i;
    done
