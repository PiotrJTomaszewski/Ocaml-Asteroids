open Sdl;;
open Common;;

type spaceship_t = {
  position: vectorf2d_t;
  speed: vectorf2d_t;
  angle: float;
}


let update_spaceship_position spaceship time_delta = {
  spaceship with
    position = {
      x = spaceship.position.x +. (spaceship.speed.x *. time_delta);
      y = spaceship.position.y +. (spaceship.speed.y *. time_delta);
    }
}


let move_spaceship spaceship action =
  let epsilon = 0.0001 in
  let new_speed = match action with
    | Up -> {
      x = spaceship.speed.x;
      y = min (spaceship.speed.y -. Constants.spaceship_speed_delta) Constants.spaceship_max_speed
    }
    | Down -> {
      x = spaceship.speed.x;
      y = min (spaceship.speed.y +. Constants.spaceship_speed_delta) Constants.spaceship_max_speed
    }
    | Left -> {
      x = min (spaceship.speed.x -. Constants.spaceship_speed_delta) Constants.spaceship_max_speed;
      y = spaceship.speed.y
    }
    | Right -> {
      x = min (spaceship.speed.x +. Constants.spaceship_speed_delta) Constants.spaceship_max_speed;
      y = spaceship.speed.y
    }
    | Fire -> spaceship.speed
  in
  let new_angle =
    (* Keep the old angle if the spaceship is not moving *)
    if abs_float new_speed.x < epsilon && abs_float new_speed.y < epsilon then
      spaceship.angle
    else
      57.32 *. atan2 new_speed.y new_speed.x +. 90.
  in
  {
    spaceship with
      speed = new_speed;
      angle = new_angle
  }


let render_spaceship renderer texture spaceship =
  Show.put_texture renderer (int_of_float spaceship.position.x, int_of_float spaceship.position.y) texture 64 64 spaceship.angle
