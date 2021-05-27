open Sdl;;

let red = (255,0,0)
let black = (0,0,0)
let alpha = 255
let fill_rect renderer (x, y) =
  let rect = Rect.make4 ~x:x ~y:y ~w:5 ~h:5 in
  Render.fill_rect renderer rect;
;;
let draw_rect renderer pos col =
  Render.set_draw_color renderer ~rgb:col ~a:alpha;
  fill_rect renderer pos;
  Render.render_present renderer;
;;
let put_texture render pos texture =
  let rect_src = Rect.make (0,0) (64,64) in
    let rect = Rect.make pos (64,64) in
    Render.copy render ~texture:texture ~src_rect:rect_src ~dst_rect:rect ();
    Render.render_present render;;


let change_pos pos da db =
  match pos with (a,b) -> (a+da,b+db)

let proc_events pos ev = 
  match ev with
    Event.KeyDown { Event.keycode = Keycode.Left;_ } -> (change_pos pos (-10) 0)
  | Event.KeyDown { Event.keycode = Keycode.Right;_ } -> change_pos pos 10 0
  | Event.KeyDown { Event.keycode = Keycode.Up;_ } -> change_pos pos 0 (-10)
  | Event.KeyDown { Event.keycode = Keycode.Down;_ } -> change_pos pos 0 10
  | Event.KeyDown { Event.keycode = Keycode.Q;_ } -> (Sdl.quit (); exit 0)
  | Event.KeyDown { Event.keycode = Keycode.Escape;_ } -> (Sdl.quit (); exit 0)
  | Event.Window_Event {Event.kind = Event.WindowEvent_Close;_} -> (Sdl.quit (); exit 0)
  | _ -> pos
;;

let rec event_loop pos =
  match Event.poll_event () with
  | None -> change_pos pos 0 1
  | Some ev ->
      let dir = proc_events pos ev in
      event_loop dir

let get_tex renderer str =
  let rw = RWops.from_file str "rb" in
  let img = Sdlimage.load_png_rw rw in
  RWops.free rw;
  Texture.create_from_surface renderer img
let load_tex renderer =
  [get_tex renderer "./res/ufo.png"]
(*main function*)
let main () =
  Sdl.init [`VIDEO];(*initialization of sdl2 subsystem*)
  Sdlimage.init [`PNG];
  let window, renderer =
    Sdl.Render.create_window_and_renderer ~width:800 ~height:600 ~flags:[] (*opening window*)
  in
  Window.set_title ~window:window ~title:"ocaml-meteors";
  (*pos: state of point (int * int) *)
  let textures = load_tex renderer in
  let rec main_loop pos befpos =
    Render.set_draw_color renderer ~rgb:black ~a:alpha;
    Render.clear renderer;
    draw_rect renderer befpos (0,255,0);
    draw_rect renderer pos red;
    put_texture renderer pos (List.hd textures);
    Timer.delay ~ms:20;
    main_loop (event_loop pos) pos
  in
  let pos = (100,100) in
  main_loop pos pos;;

let () = main ()
