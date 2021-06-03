open Sdl;;
open Common;;


let close () = (Sdl.quit (); Sdlimage.quit ();Sdlttf.quit ();exit 0)

let proc_events game ev = 
  match ev with
    Event.KeyDown { Event.keycode = Keycode.Left;_ } -> Game.process_inputs game Left
  | Event.KeyDown { Event.keycode = Keycode.Right;_ } -> Game.process_inputs game Right
  | Event.KeyDown { Event.keycode = Keycode.Up;_ } -> Game.process_inputs game Up
  | Event.KeyDown { Event.keycode = Keycode.Down;_ } -> Game.process_inputs game Down
  | Event.KeyDown { Event.keycode = Keycode.Space;_ } -> Game.process_inputs game Fire
  | Event.KeyDown { Event.keycode = Keycode.Q;_ } -> close ()
  | Event.KeyDown { Event.keycode = Keycode.R;_ } -> (Game.init ())
  | Event.KeyDown { Event.keycode = Keycode.Escape;_ } -> close ()
  | Event.Window_Event {Event.kind = Event.WindowEvent_Close;_} -> close ()
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
  Sdlttf.init ();
  let font = (Sdlttf.open_font ("./res/font.ttf") 12) in
  let window, renderer =
    Sdl.Render.create_window_and_renderer ~width:Constants.window_width ~height:Constants.window_height ~flags:[] (*opening window*)
  in
  Window.set_title ~window:window ~title:"ocaml-asteroids";
  (*pos: state of point (int * int) *)
  let textures = Show.load_tex renderer 
      ["ufo"; "meteor"; "bullet"] 
      ["./res/ufo.png";"./res/meteor.png"; "./res/placeholder.png"] 
  in
  let rec main_loop game1 =
    let game  = (Game.check_shots game1)in
    match Game.check_collision game with
    true -> main_loop (
      Game.handle_spaceship_crash game
    ) 
    | _ -> Render.set_draw_color renderer ~rgb:Show.black ~a:Show.alpha;
    Render.clear renderer;
    Game.render_game renderer textures game font;
    if Game.is_game_over game then (
      let game_over_surf = Sdlttf.render_text_solid font "Game Over" {r = 255; g=255; b=255; a=255;} in
      Show.draw_rect_surf renderer game_over_surf ((Constants.window_width/2),(Constants.window_height/2 - 50)) Show.black (10 * 18 + 70,40);
      Surface.free game_over_surf;
      let restart_surf = Sdlttf.render_text_solid font "Press R" {r = 255; g=255; b=255; a=255;} in
      Show.draw_rect_surf renderer restart_surf ((Constants.window_width/2),(Constants.window_height/2 + 50)) Show.black (8 * 18 + 70,40);
      Surface.free restart_surf
    );

    Render.render_present renderer;
    let delay = 16 in
    Timer.delay ~ms:delay;
    main_loop ( (event_loop game delay))

  in
    main_loop (Game.init ());;

let () = main ()
