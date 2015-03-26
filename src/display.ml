type position = int * int

let sprites_path = "resources/sprites.png"
let lang_sprites_path = "resources/sprites_lang.png"
let font_path = "resources/font.ttf"

let screen = ref (Obj.magic None)
let sprites = ref (Obj.magic None)
let sprites_lang = ref (Obj.magic None)
let sprite_cursor = ref (Obj.magic None)
let cell_size = ref 32
let lang_cell_size = 32

let font = ref (Obj.magic None)

let init width height c_size =
    Sdl.init [`VIDEO];
    screen := Sdlvideo.set_video_mode ~w: width ~h: height ~bpp: 32
                                      [`DOUBLEBUF; `HWSURFACE];
    cell_size := c_size;
    sprites := Sdlvideo.create_RGB_surface_format !screen [`HWSURFACE]
                                                  ~w: (5 * c_size) ~h: (5 * c_size);
    let imgs = Sdlloader.load_image sprites_path in
    let imgs = Sdlgfx.zoomSurface imgs
                                  ((float_of_int c_size) /. 32.)
                                  ((float_of_int c_size) /. 32.)
                                  true
    in
    Sdlvideo.blit_surface
        ~src: imgs
        ~src_rect: (Sdlvideo.rect 0 0 (5 * c_size) (5 * c_size))
        ~dst: !sprites
        ~dst_rect: (Sdlvideo.rect 0 0 (5 * c_size) (5 * c_size))
        ();
    Sdlvideo.set_color_key !sprites (Sdlvideo.map_RGB !sprites (82, 123, 156));
    sprites_lang := Sdlloader.load_image lang_sprites_path;
    Sdlvideo.set_color_key !sprites_lang 
                           (Sdlvideo.map_RGB !sprites_lang (82, 123, 156));
    sprite_cursor := Sdlvideo.create_RGB_surface_format !screen [`HWSURFACE]
                                                        ~w: 36 ~h: 36;
    Sdlvideo.fill_rect
        ~rect: (Sdlvideo.rect 0 0 36 36)
        !sprite_cursor
        (Sdlvideo.map_RGB !sprite_cursor Sdlvideo.yellow);
    Sdlttf.init ();
    font := Sdlttf.open_font font_path 50

let close () =
    Sdl.quit ()

let clear () =
    Sdlvideo.fill_rect !screen (Sdlvideo.map_RGB !screen Sdlvideo.black)

let blit_sprite ((x,y): position) (idx: int) (idy: int) =
    Sdlvideo.blit_surface
        ~src: !sprites
        ~src_rect: (Sdlvideo.rect (idx * !cell_size) (idy * !cell_size)
                                  !cell_size !cell_size)
        ~dst: !screen
        ~dst_rect: (Sdlvideo.rect x y !cell_size !cell_size)
        ()

let draw_cell (pos: position) (color: (Puzzle.color option)) =
    Puzzle.(match color with
    | Some Blue  -> blit_sprite pos 0 0
    | Some Red   -> blit_sprite pos 1 0
    | Some Green -> blit_sprite pos 2 0
    | None -> blit_sprite pos 3 0
    )

let draw_cursor ((x,y): position) =
    Sdlvideo.blit_surface
        ~src: !sprite_cursor
        ~src_rect: (Sdlvideo.rect 0 0 36 36)
        ~dst: !screen
        ~dst_rect: (Sdlvideo.rect x y 36 36)
        ()

let draw_star (pos: position) =
      blit_sprite pos 4 0

let robot_steps = 8
let robot_sprites = [2;3;4;3;2;1;0;1]
let robot_nb_sprites = List.length robot_sprites

let draw_robot (pos: position) (dir: Puzzle.direction) (mstep: int) =
    let sx = List.nth robot_sprites (mstep mod robot_nb_sprites)
    and sy = Puzzle.(match dir with
        | South -> 1
        | East  -> 2
        | North -> 3
        | West  -> 4
    )
    in blit_sprite pos sx sy

let draw_arrow ((x,y): position) (dir: Puzzle.direction) =
    let idx = Puzzle.(match dir with
        | North -> 0
        | West  -> 1
        | East  -> 2
        | South -> failwith "should not happen..."
    )
    in Sdlvideo.blit_surface
        ~src: !sprites_lang
        ~src_rect: (Sdlvideo.rect (idx * 32) 0 32 32)
        ~dst: !screen
        ~dst_rect: (Sdlvideo.rect x y 32 32)
        ()

let draw_call ((x,y): position) (f: string) =
    let idx = match f with
        | "f1" -> 3
        | "f2" -> 4
        | "f3" -> 5
        | "f4" -> 6
        | "f5" -> 7
        | _ -> failwith "unknown function name"
    in Sdlvideo.blit_surface
        ~src: !sprites_lang
        ~src_rect: (Sdlvideo.rect (idx * 32) 0 32 32)
        ~dst: !screen
        ~dst_rect: (Sdlvideo.rect x y 32 32)
        ()

let draw_text ((x,y): position) (s: string) =
    let surf = Sdlttf.render_text_solid !font s Sdlvideo.yellow  in
    Sdlvideo.blit_surface
        ~src: surf
        ~dst: !screen
        ~dst_rect: (Sdlvideo.rect x y 0 0)
        ()

let sync () =
    Sdlvideo.flip !screen

let delay ms =
    Sdltimer.delay ms
