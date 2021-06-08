open Sdl


(* Record type describing state of game *)
type game_t = {
  score: int;
  lifes: int;
  rounds: int;
  spaceship: Spaceship.spaceship_t;
  meteors: Meteor.meteor_t list;
  bullets: Bullet.bullet_t list;
}


(* Generates a list of meteors *)
let rec spawn_meteors counter = match counter with
  | 0 -> [Meteor.spawn_meteor ()];
  | _ -> (spawn_meteors (counter-1))@[Meteor.spawn_meteor ()];
;;


(*initial state*)
let init () = {
  rounds = 1;
  score = 0;
  lifes = 3;
  spaceship = {
    position = {x = float_of_int (Constants.window_width/2); y = float_of_int (Constants.window_height/2);};
    speed = {x = 0.; y = 0.;};
    angle = 0.;
  };
  meteors = spawn_meteors (Utils.random_int Constants.min_meteor_init_count Constants.min_meteor_init_count);
  bullets = [];
}


(* Checks whether game is over *)
let is_game_over game =
  game.lifes < 0


let handle_spaceship_crash game = 
  let new_game = {
    rounds = game.rounds;
    score = game.score;
    lifes = game.lifes - 1;
    spaceship = {
      position = {x = float_of_int (Constants.window_width/2); y = float_of_int (Constants.window_height/2);};
      speed = {x = 0.; y = 0.;};
      angle = 0.;
    };
    meteors = spawn_meteors (Utils.random_int Constants.min_meteor_init_count Constants.min_meteor_init_count);
    bullets = [];
  }
  in
  (* If the game is over place the spaceship outside the screen *)
  if is_game_over new_game then
    {new_game with spaceship = {new_game.spaceship with position = {x = -.1000.; y = -.1000.}}}
  else
    new_game


(* Putting score and lifes *)
let draw_text renderer game font=
  Show.draw_rect renderer (Constants.window_width/2,15) (50,50,50) (Constants.window_width,30);
  let length = Int.of_float (log10 (Float.of_int game.score)) + 1 in (*length of number*)
  let score_surf = Sdlttf.render_text_solid font (String.concat ""  ["Score:";string_of_int game.score]) {r = 255; g=255; b=255; a=255;} in
  Show.draw_rect_surf renderer score_surf ((Constants.window_width/4) * 3,15) Show.black (length * 18 + 70,40);
  Surface.free score_surf;
  let life_surf = Sdlttf.render_text_solid font (String.concat "" ["Lifes:";string_of_int game.lifes]) {r = 255; g=255; b=255; a=255;} in
  Show.draw_rect_surf renderer life_surf ((Constants.window_width/6),15) Show.black (98,40);
  Surface.free life_surf;
  let round_surf = Sdlttf.render_text_solid font (String.concat "" ["Round:";string_of_int game.rounds]) {r = 255; g=255; b=255; a=255;} in
  Show.draw_rect_surf renderer round_surf ((Constants.window_width/2),15) Show.black (98,40);
  Surface.free round_surf
;;


let render_game renderer textures game font =
  let meteor_texture = (Show.TexMap.find "meteor" textures) in
  let bullet_texture = (Show.TexMap.find "bullet" textures) in
  Utils.for_each (Meteor.render_meteor renderer meteor_texture) game.meteors;
  Utils.for_each (Bullet.render_bullet renderer bullet_texture) game.bullets;
  draw_text renderer game font;
  Spaceship.render_spaceship renderer (Show.TexMap.find "ufo" textures) game.spaceship;
  ()


(* Moving everything *)
let update_time game time_delta =
  let (new_meteors, new_round) = match game.meteors with
  | [] -> (spawn_meteors (Random.int (Constants.max_metor_init_count - Constants.min_meteor_init_count) + Constants.min_meteor_init_count),game.rounds+1)
  | _  -> (game.meteors,game.rounds)
  in
  let time_delta_float = float_of_int time_delta in
  (* Disable spaceship movement when the game is over *)
  let new_spaceship =
    if is_game_over game then
      game.spaceship
    else
      Spaceship.update_spaceship_position game.spaceship time_delta_float
  in
  {
    game with
      rounds = new_round;
      spaceship = new_spaceship;
      meteors = List.map (fun m -> Meteor.update_meteor_position m time_delta_float) new_meteors;
      bullets = List.filter_map (fun b -> Bullet.update_bullet_position b time_delta_float) game.bullets
  }


(* Changing state of game due to action caused by ecent in Main.proc_events *)
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


(*
  Collisions are detected with simple approximation. Objects are circles and it is checked if
    sum of 2 radii is is bigger then distance betwen centers of objects with tolerance in pixels
*)
let in_collision r1 r2 (x,y) (a,b) tolerancy = (sqrt ((x-.a)*.(x-.a)+.(y-.b)*.(y-.b))) < (float_of_int (r1+r2-tolerancy))


(* Checking collision of spaceship and meteor *)
let check_collision (game:game_t) =
  let ship_vs_meteor (meteor:Meteor.meteor_t) = 
    let size = meteor.size * Constants.meteor_size_scale in
    in_collision (Constants.spaceship_size/2) (size/2) 
      (Common.pair_float_of_vectorf2d_t game.spaceship.position)
      (Common.pair_float_of_vectorf2d_t meteor.position)  (4) in
  let collided = List.map (ship_vs_meteor) game.meteors in
  List.fold_right (|| ) collided false
  ;;


(* Checking collision of meteor and bullet *)
let bullet_vs_meteor  (bullet:Bullet.bullet_t) (meteor:Meteor.meteor_t) = 
    let size = meteor.size * Constants.meteor_size_scale in
    let condition = in_collision 16 (size/2) 
      (Common.pair_float_of_vectorf2d_t bullet.position)
      (Common.pair_float_of_vectorf2d_t meteor.position)  8 in 
  (Meteor.split_meteor_on_collision meteor condition,condition)


(* Checking for if given bullet hit any meteor *)
let check_meteors meteors bullet =
  let mapped = List.map (bullet_vs_meteor bullet) meteors in
  List.fold_left (fun ((l1, score1), b) ((l2, score2), cond) -> ((l1@l2,score1+score2),cond||b)) (([], 0), false) mapped 
  

(* Aplying check_meteors for every bullet *)
let check_bullets bullets meteors score = 
  let rec check_bullets_rec meteors_rec score = function
  [] -> (meteors_rec, score, [])
  | hd::tl -> let ((new_meteors, score_delta), condition) = check_meteors meteors_rec hd in
      if condition then 
        check_bullets_rec new_meteors (score+score_delta) tl 
      else 
        let (meteors_stable, score, rest_of_bullets) = check_bullets_rec new_meteors score tl in 
          (meteors_stable, score, hd::rest_of_bullets)
  in
  check_bullets_rec meteors score bullets


(* Exexcuting check_bullets with data from state of game *)
let check_shots (game:game_t) =
  let (new_meteors, new_score, new_bullets) = check_bullets game.bullets game.meteors game.score in 
  {game with meteors=new_meteors; score=new_score; bullets=new_bullets};
