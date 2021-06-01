open Sdl

type game_t = {
  score: int;
  lifes: int;
  spaceship: Spaceship.spaceship_t;
  meteors: Meteor.meteor_t list;
  bullets: Bullet.bullet_t list;
}


let rec init_meteors counter = match counter with
  | 0 -> [Meteor.spawn_meteor ()];
  | _ -> (init_meteors (counter-1))@[Meteor.spawn_meteor ()];
;;


let init () = {
  score = 0;
  lifes = 3;
  spaceship = {
    position = {x = float_of_int (Constants.window_width/2); y = float_of_int (Constants.window_height/2);};
    speed = {x = 0.; y = 0.;};
    angle = 0.;
  };
  meteors = init_meteors (Random.int (Constants.max_metor_init_count - Constants.min_meteor_init_count) + Constants.min_meteor_init_count);
  bullets = [];
}


let render_game renderer textures game =
  let meteor_texture = (Show.TexMap.find "meteor" textures) in
  let bullet_texture = (Show.TexMap.find "bullet" textures) in
  Utils.for_each (Meteor.render_meteor renderer meteor_texture) game.meteors;
  Utils.for_each (Bullet.render_bullet renderer bullet_texture) game.bullets;
  Spaceship.render_spaceship renderer (Show.TexMap.find "ufo" textures) game.spaceship;
  ()


let update_time game time_delta = 
  let time_delta_float = float_of_int time_delta in
  {
    game with
      spaceship = Spaceship.update_spaceship_position game.spaceship time_delta_float;
      meteors = List.map (fun m -> Meteor.update_meteor_position m time_delta_float) game.meteors;
      bullets = List.map (fun b -> Bullet.update_bullet_position b time_delta_float) game.bullets
  }


let process_inputs game action =
  let new_spaceship = Spaceship.move_spaceship game.spaceship action in
  let new_bullets =
    if action == Common.Fire then
      game.bullets
      @[Bullet.spawn_bullet new_spaceship.position new_spaceship.angle]
    else
      game.bullets
  in
  {
    game with
      spaceship = new_spaceship;
      bullets = new_bullets
  }
