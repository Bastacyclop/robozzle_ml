type pos = int * int

let screen = ref (Obj.magic None)
let sprites = ref (Obj.magic None)
let sprites_lang = ref (Obj.magic None)
let sprite_cursor = ref (Obj.magic None)
let cell_size = ref 32
let lang_cell_size = 32

let font = ref (Obj.magic None)

let init (w : int) (h : int) (c : int) : unit =
  Sdl.init [`VIDEO];
  screen := Sdlvideo.set_video_mode ~w:w ~h:h ~bpp:32 [`DOUBLEBUF ; `HWSURFACE];
  cell_size := c;
  sprites := Sdlvideo.create_RGB_surface_format !screen [`HWSURFACE]
    ~w:(5 * c) ~h:(5 * c);
  let imgs = Sdlloader.load_image "sprites.png" in
  let imgs = Sdlgfx.zoomSurface imgs
    ((float_of_int c) /. 32.)
    ((float_of_int c) /. 32.)
    true
  in
  Sdlvideo.blit_surface
    ~src:imgs
    ~src_rect:(Sdlvideo.rect 0 0 (5 * c) (5 * c))
    ~dst:!sprites
    ~dst_rect:(Sdlvideo.rect 0 0 (5 * c) (5 * c))
    ();
  Sdlvideo.set_color_key !sprites (Sdlvideo.map_RGB !sprites (82, 123, 156));
  sprites_lang := Sdlloader.load_image "sprites_lang.png";
  Sdlvideo.set_color_key !sprites_lang (Sdlvideo.map_RGB !sprites_lang (82, 123, 156));
  sprite_cursor := Sdlvideo.create_RGB_surface_format !screen [`HWSURFACE] ~w:36 ~h:36;
  Sdlvideo.fill_rect
    ~rect:(Sdlvideo.rect 0 0 36 36)
    !sprite_cursor
    (Sdlvideo.map_RGB !sprite_cursor Sdlvideo.yellow);
  Sdlttf.init ();
  font := Sdlttf.open_font "font.ttf" 50
  

let quit () : unit =
  Sdl.quit ()

let clear () : unit =
  Sdlvideo.fill_rect !screen (Sdlvideo.map_RGB !screen Sdlvideo.black)

let blit_sprite ((x,y) : pos) (idx : int) (idy : int) : unit =
  Sdlvideo.blit_surface
    ~src:!sprites
    ~src_rect:(Sdlvideo.rect (idx * !cell_size) (idy * !cell_size) !cell_size !cell_size)
    ~dst:!screen
    ~dst_rect:(Sdlvideo.rect x y !cell_size !cell_size)
    ()

let draw_cell (pos : pos) (color : Puzzle.color) : unit =
  match color with
    | Puzzle.Blue  -> blit_sprite pos 0 0
    | Puzzle.Red   -> blit_sprite pos 1 0
    | Puzzle.Green -> blit_sprite pos 2 0
    | Puzzle.Empty -> blit_sprite pos 3 0

let draw_cursor ((x,y) : pos) : unit =
  Sdlvideo.blit_surface
    ~src:!sprite_cursor
    ~src_rect:(Sdlvideo.rect 0 0 36 36)
    ~dst:!screen
    ~dst_rect:(Sdlvideo.rect x y 36 36)
    ()

let draw_star (pos : pos) : unit =
  blit_sprite pos 4 0

let robot_steps = 8
let robot_sprites    = [2;3;4;3;2;1;0;1]
let robot_nb_sprites = List.length robot_sprites

let draw_robot (pos : pos) (dir : Puzzle.direction) (mstep : int) : unit =
  let sx = List.nth robot_sprites (mstep mod robot_nb_sprites)
  and sy = match dir with
    | Puzzle.South -> 1
    | Puzzle.East  -> 2
    | Puzzle.North -> 3
    | Puzzle.West  -> 4
  in blit_sprite pos sx sy

let draw_arrow ((x,y) : pos) (dir : Puzzle.direction) : unit =
  let idx = match dir with
    | Puzzle.North -> 0
    | Puzzle.West  -> 1
    | Puzzle.East  -> 2
    | Puzzle.South -> failwith "should not happen..."
  in Sdlvideo.blit_surface
    ~src:!sprites_lang
    ~src_rect:(Sdlvideo.rect (idx * 32) 0 32 32)
    ~dst:!screen
    ~dst_rect:(Sdlvideo.rect x y 32 32)
    ()

let draw_call ((x,y) : pos) (f : string) : unit =
  let idx = match f with
    | "f1" -> 3
    | "f2" -> 4
    | "f3" -> 5
    | "f4" -> 6
    | "f5" -> 7
    | _ -> failwith "unknown function name"
  in Sdlvideo.blit_surface
    ~src:!sprites_lang
    ~src_rect:(Sdlvideo.rect (idx * 32) 0 32 32)
    ~dst:!screen
    ~dst_rect:(Sdlvideo.rect x y 32 32)
    ()

let draw_text ((x,y) : pos) (s : string) : unit =
  let surf = Sdlttf.render_text_solid !font s Sdlvideo.yellow  in
  Sdlvideo.blit_surface
    ~src:surf
    ~dst:!screen
    ~dst_rect:(Sdlvideo.rect x y 0 0)
    ()
    
let sync () : unit =
  Sdlvideo.flip !screen

let delay (u : int) : unit =
  Sdltimer.delay u
