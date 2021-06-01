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
let draw_text renderer game font=

  Show.draw_rect renderer (Constants.window_width/2,15) (50,50,50) (Constants.window_width,30);
  let length = Int.of_float (log10 (Float.of_int game.score)) + 1 in
  let score_surf = Sdlttf.render_text_solid font (String.concat ""  ["Score:";string_of_int game.score]) {r = 255; g=255; b=255; a=255;} in
  Show.draw_rect_surf renderer score_surf ((Constants.window_width/4) * 3,15) Show.black (length * 18 + 70,40);
  Surface.free score_surf;
  let life_surf = Sdlttf.render_text_solid font (String.concat "" ["Lifes:";string_of_int game.lifes]) {r = 255; g=255; b=255; a=255;} in
  Show.draw_rect_surf renderer life_surf ((Constants.window_width/6),15) Show.black (98,40);
  Surface.free life_surf
;;

let render_game renderer textures game font =
  let meteor_texture = (Show.TexMap.find "meteor" textures) in
  let bullet_texture = (Show.TexMap.find "bullet" textures) in
  Utils.for_each (Meteor.render_meteor renderer meteor_texture) game.meteors;
  Utils.for_each (Bullet.render_bullet renderer bullet_texture) game.bullets;
  draw_text renderer game font;
  Spaceship.render_spaceship renderer (Show.TexMap.find "ufo" textures) game.spaceship;
  ()


let update_time game time_delta = 
  let time_delta_float = float_of_int time_delta in
  {
    game with
      spaceship = Spaceship.update_spaceship_position game.spaceship time_delta_float;
      meteors = List.map (fun m -> Meteor.update_meteor_position m time_delta_float) game.meteors;
      bullets = List.filter_map (fun b -> Bullet.update_bullet_position b time_delta_float) game.bullets
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
;;
let in_collision r1 r2 (x,y) (a,b) = (sqrt (Float.of_int ((x-a)*(x-a)+(y-b)*(y-b))) < (float_of_int (r1+r2)))
(*
  
*)
let check_collision (game:game_t) =
  let ship_vs_meteor (meteor:Meteor.meteor_t) = 
    let size = meteor.size * Constants.meteor_size_scale in
    in_collision (Constants.spaceship_size/2) (size/2) 
      (int_of_float game.spaceship.position.x,int_of_float game.spaceship.position.y)
      (int_of_float meteor.position.x,int_of_float meteor.position.y) in
  let collided = List.map (ship_vs_meteor) game.meteors in
  List.fold_right (|| ) collided false