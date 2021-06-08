open Common;;


type spaceship_t = {
  position: vectorf2d_t;
  speed: vectorf2d_t;
  angle: float;
}


(* Calculate spaceship position on a new frame *)
let update_spaceship_position spaceship time_delta =
  (* In the original game when the spaceship leaves the screen it teleports to the opposite site *)
  {
    spaceship with
      position =
        Utils.update_position_with_warp spaceship.position spaceship.speed (float_of_int Constants.spaceship_size) time_delta
      ;
  }


(* Change spaceship speed and angle in accordance with the input action *)
let move_spaceship spaceship action =
  let new_speed = match action with
    | Up -> {
      x = spaceship.speed.x;
      y =  Utils.speed_limit (spaceship.speed.y -. Constants.spaceship_speed_delta) Constants.spaceship_max_speed
    }
    | Down -> {
      x = spaceship.speed.x;
      y = Utils.speed_limit (spaceship.speed.y +. Constants.spaceship_speed_delta) Constants.spaceship_max_speed
    }
    | Left -> {
      x = Utils.speed_limit (spaceship.speed.x -. Constants.spaceship_speed_delta) Constants.spaceship_max_speed;
      y = spaceship.speed.y
    }
    | Right -> {
      x = Utils.speed_limit (spaceship.speed.x +. Constants.spaceship_speed_delta) Constants.spaceship_max_speed;
      y = spaceship.speed.y
    }
    | Fire -> spaceship.speed
  in
  let new_angle =
    (* Keep the old angle if the spaceship is not moving *)
    if abs_float new_speed.x < Float.epsilon && abs_float new_speed.y < Float.epsilon then
      spaceship.angle
    else
      (* 180 / PI ~= 57.32 *)
      57.32 *. atan2 new_speed.y new_speed.x +. 90.
  in
  {
    spaceship with
      speed = new_speed;
      angle = new_angle
  }


let render_spaceship renderer texture spaceship =
  Show.put_texture renderer (int_of_float spaceship.position.x, int_of_float spaceship.position.y) texture Constants.spaceship_size Constants.spaceship_size spaceship.angle
