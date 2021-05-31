open Sdl

type meteor_t = {
  position: Common.vectorf2d_t;
  speed: Common.vectorf2d_t;
  (* color: Sdlpixel.rgb; *)
  size: int;
}


let generate_meteor () =
  (* Bound parameter in Random is exclusive hence +1 everywhere *)
  let size = (Random.int (Constants.max_meteor_init_size)) + 1 in
  let max_x = float_of_int (Constants.window_width - (size * Constants.meteor_size_scale)) in
  let max_y = float_of_int (Constants.window_height - (size * Constants.meteor_size_scale)) in
  {
    (* Meteors can spawn only on the edges of the screen *)
    position = (
      match Random.int 4 with
      | 0 -> {x = 0.; y = Random.float (max_y +. 1.)};
      | 1 -> {x = max_x; y = Random.float (max_y +. 1.)};
      | 2 -> {x = Random.float (max_x +. 1.); y = 0.};
      | _ -> {x = Random.float (max_x +. 1.); y = max_y};
    );
    speed = {
      x = (Random.float (Constants.max_meteor_speed -. Constants.min_meteor_speed)) +. Constants.min_meteor_speed;
      y = (Random.float (Constants.max_meteor_speed -. Constants.min_meteor_speed)) +. Constants.min_meteor_speed
    };
    size = size;
  }


let update_meteor_position meteor time_delta =
  let meteor_pixel_size = float_of_int(meteor.size * Constants.meteor_size_scale) in
  let calculated_position_x = meteor.position.x +. (meteor.speed.x *. time_delta) in
  let calculated_position_y = meteor.position.y +. (meteor.speed.y *. time_delta) in
  let new_position_x =
    if calculated_position_x < (-. meteor_pixel_size) then
      float_of_int(Constants.window_width)
    else if calculated_position_x > float_of_int(Constants.window_width) then
      0.
    else
      calculated_position_x
  in
  let new_position_y =
    if calculated_position_y < (-. meteor_pixel_size) then
      float_of_int(Constants.window_height)
    else if calculated_position_y > float_of_int(Constants.window_height) then
      0.
    else
      calculated_position_y
  in
  {
    meteor with
      position = {
        x = new_position_x;
        y = new_position_y;
      };
  }


let render_meteor renderer texture meteor =
  Show.put_texture renderer  (int_of_float meteor.position.x, int_of_float(meteor.position.y)) texture 64 64 45.0
    (* (meteor.size * Constants.meteor_size_scale)
    (meteor.size * Constants.meteor_size_scale) *)
