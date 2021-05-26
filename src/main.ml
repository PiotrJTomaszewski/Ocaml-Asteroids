open Tsdl


let init_engine () =
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok () -> ()


let init_display () =
  match Sdl.create_window_and_renderer ~w: 800 ~h: 600 Sdl.Window.opengl with
  | Error (`Msg e) -> Sdl.log "Create window and renderer error %s" e; exit 1
  | Ok (window, renderer) -> ();
  Sdl.set_window_title window "Asteroids";
  (window, renderer)


let cleanup () =
  print_endline "Cleanup";
  Sdl.quit ()


let main () =
  at_exit cleanup; 
  init_engine ();
  let window, renderer = init_display () in
  Sdl.delay 1000l


let () = main ()
