open Sdl;;

(* let change_pos pos da db =
  match pos with (a,b) -> (a+da,b+db) *)


let proc_events game ev = 
  match ev with
    Event.KeyDown { Event.keycode = Keycode.Left;_ } -> game
  | Event.KeyDown { Event.keycode = Keycode.Right;_ } -> game
  | Event.KeyDown { Event.keycode = Keycode.Up;_ } -> game
  | Event.KeyDown { Event.keycode = Keycode.Down;_ } -> game
  | Event.KeyDown { Event.keycode = Keycode.Q;_ } -> (Sdl.quit (); exit 0)
  | Event.KeyDown { Event.keycode = Keycode.Escape;_ } -> (Sdl.quit (); exit 0)
  | Event.Window_Event {Event.kind = Event.WindowEvent_Close;_} -> (Sdl.quit (); exit 0)
  | _ -> game
;;

let rec event_loop game time_delta =
  match Event.poll_event () with
  | None -> Game.update_time game time_delta
  | Some ev ->
    event_loop (proc_events game ev) time_delta

(*main function*)
let main () =
  Sdl.init [`VIDEO];(*initialization of sdl2 subsystem*)
  Sdlimage.init [`PNG];
  let window, renderer =
    Sdl.Render.create_window_and_renderer ~width:Constants.window_width ~height:Constants.window_height ~flags:[] (*opening window*)
  in
  Window.set_title ~window:window ~title:"ocaml-asteroids";
  (*pos: state of point (int * int) *)
  let textures = Show.load_tex renderer 
      ["ufo"; "meteor"] 
      ["./res/ufo.png";"./res/meteor.png";] 
  in
  let rec main_loop game =
    Render.set_draw_color renderer ~rgb:Show.black ~a:Show.alpha;
    Render.clear renderer;
    Game.render_game renderer textures game;
    Render.render_present renderer;
    Timer.delay ~ms:16;
    main_loop (event_loop game 16)
  in
  (* let pos = (100,100) in *)
    main_loop (Game.init ());;

let () = main ()
