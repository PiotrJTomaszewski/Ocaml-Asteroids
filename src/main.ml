open Sdl;;



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

(*main function*)
let main () =
  Sdl.init [`VIDEO];(*initialization of sdl2 subsystem*)
  Sdlimage.init [`PNG];
  let window, renderer =
    Sdl.Render.create_window_and_renderer ~width:800 ~height:600 ~flags:[] (*opening window*)
  in
  Window.set_title ~window:window ~title:"ocaml-asteroids";
  (*pos: state of point (int * int) *)
  let textures = Show.load_tex renderer 
      ["ufo"; "meteor"] 
      ["./res/ufo.png";"./res/meteor.png";] 
  in
  let rec main_loop state =
    Render.set_draw_color renderer ~rgb:Show.black ~a:Show.alpha;
    Render.clear renderer;
    Show.put_texture renderer state (Show.TexMap.find "ufo" textures) 64 64 45.0;
    Timer.delay ~ms:16;
    main_loop (event_loop state)
  in
  let pos = (100,100) in
  main_loop pos;;

let () = main ()
