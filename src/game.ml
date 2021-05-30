open Sdl

type game_t = {
  score: int;
  lifes: int;
  spaceship: Spaceship.spaceship_t;
  meteors: Meteor.meteor_t list;
}


let rec init_meteors counter = match counter with
  | 0 -> [Meteor.generate_meteor ()];
  | _ -> (init_meteors (counter-1))@[Meteor.generate_meteor ()];
;;


let rec render_all_meteors renderer meteors = match meteors with
  | [] -> ();
  | hd :: tl -> Meteor.render_meteor renderer hd; render_all_meteors renderer tl;
;;


let init () = {
  score = 0;
  lifes = 3;
  spaceship = {
    position = {x = 400.; y = 300.;};
    speed = {x = 0.05; y = -0.05;};
  };
  meteors = init_meteors (Random.int Constants.max_metor_init_count);
}


let render_game renderer game =
  Render.set_draw_color renderer (100, 100, 100) 255;
  Render.clear renderer;
  Spaceship.render_spaceship renderer game.spaceship;
  render_all_meteors renderer game.meteors;
  Render.render_present renderer;
  ()


let update_time game time_delta = {
  game with
    spaceship = Spaceship.update_spaceship_position game.spaceship time_delta
}
