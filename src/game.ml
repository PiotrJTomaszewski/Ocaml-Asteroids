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


let init () = {
  score = 0;
  lifes = 3;
  spaceship = {
    position = {x = float_of_int (Constants.window_width/2); y = float_of_int (Constants.window_height/2);};
    speed = {x = 0.; y = 0.;};
    angle = 0.;
  };
  meteors = init_meteors (Random.int Constants.max_metor_init_count);
}


let render_game renderer textures game =
  Spaceship.render_spaceship renderer (Show.TexMap.find "ufo" textures) game.spaceship;
  let meteor_texture = (Show.TexMap.find "meteor" textures) in
  Utils.for_each (Meteor.render_meteor renderer meteor_texture) game.meteors;
  Render.render_present renderer;
  ()


let update_time game time_delta = 
  let time_delta_float = float_of_int time_delta in
  {
    game with
      spaceship = Spaceship.update_spaceship_position game.spaceship time_delta_float;
      meteors = List.map (fun m -> Meteor.update_meteor_position m time_delta_float) game.meteors
  }


let process_inputs game action =
  {
    game with
    spaceship = Spaceship.move_spaceship game.spaceship action
  }