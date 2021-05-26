open Tsdl

let log fmt = Format.printf "%a\n" fmt;;

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


let key_event_handler event up_down =
  let scancode = Sdl.Event.(get event keyboard_scancode) in
  if up_down == Sdl.Event.key_up then  
    match Sdl.Scancode.(enum scancode) with
    | `W -> print_endline "W pressed";
    | `S -> print_endline "S pressed";
    | `A -> print_endline "A pressed";
    | `D -> print_endline "D pressed";
    | `Space -> print_endline "Space pressed";
    | `Escape -> print_endline "Exit pressed"; exit 0;
    | _ -> ();
  else
    match Sdl.Scancode.(enum scancode) with
    | `W -> print_endline "W released";
    | `S -> print_endline "S released";
    | `A -> print_endline "A released";
    | `D -> print_endline "D released";
    | `Space -> print_endline "Space released";
    | _ -> ();
  ()


let poll_events () =
  let event = Sdl.Event.create () in
  while Sdl.poll_event (Some event) do
    match Sdl.Event.(enum (get event typ)) with
    | `Quit -> print_endline "Quit Event"; exit 0;
    | `Key_down -> key_event_handler event Sdl.Event.key_up;
    | `Key_up -> key_event_handler event Sdl.Event.key_down;
    | _ -> ();
  done;
  ()


let main () =
  at_exit cleanup; 
  init_engine ();
  let window, renderer = init_display () in
  let rec main_loop () =
    poll_events ();
    Sdl.delay 300l;
    main_loop();
  in
  main_loop()


let () = main ()
