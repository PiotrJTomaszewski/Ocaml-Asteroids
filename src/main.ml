open Sdl;;
open Common;;


(*closing game*)
let close () = (Sdl.quit (); Sdlimage.quit ();Sdlttf.quit ();exit 0)


(*checking value of event ev and genering next state of game*)
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


(*checking if there is any event*)
let rec event_loop game time_delta =
  match Event.poll_event () with
  | None -> Game.update_time game time_delta
  | Some ev ->
    event_loop (proc_events game ev) time_delta (*executing the action for catched event*)
  

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
  let textures = Show.load_tex renderer (*loading textures*)
      ["ufo"; "meteor"; "bullet"] 
      ["./res/ufo.png";"./res/meteor.png"; "./res/laser.png"] 
  in
  let rec main_loop game1 last_millis =
    let game  = Game.check_shots game1 in (*checking if any bullet has shot any meteor*)
    if Game.check_collision game then main_loop (
          Game.handle_spaceship_crash game
        ) last_millis
    else 
      Render.set_draw_color renderer ~rgb:Show.black ~a:Show.alpha; 
      Render.clear renderer;
      Game.render_game renderer textures game font;
      if Game.is_game_over game then (
        Show.game_over_text renderer font;
      );
      Render.render_present renderer;
      let current_millis = Sdltimer.get_ticks () in
      let loop_time = current_millis- last_millis in
      main_loop (event_loop game loop_time) current_millis 

  in
    main_loop (Game.init ()) (Sdltimer.get_ticks ());;


let () = main ()
