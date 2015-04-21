val should_quit: unit -> bool
val should_expose: unit -> bool

val handle: ?on_key_pressed:(Sdlkey.t -> unit) -> unit -> unit

val wait: (Sdlevent.event -> 'a option) -> 'a
val wait_key: (Sdlkey.t -> 'a option) -> 'a
