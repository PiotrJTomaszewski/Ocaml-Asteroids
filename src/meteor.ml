type meteor_t = {
  position: Common.vectorf2d_t;
  speed: Common.vectorf2d_t;
  (* color: Sdlpixel.rgb; *)
  size: int;
}


let spawn_meteor () =
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
  let new_position = Utils.update_position_with_warp meteor.position meteor.speed meteor_pixel_size time_delta in
  {
    meteor with
      position = {
        x = new_position.x;
        y = new_position.y;
      };
  }


let render_meteor renderer texture meteor =
  let meteor_pixel_size = meteor.size * Constants.meteor_size_scale in
  Show.put_texture renderer  (int_of_float meteor.position.x, int_of_float(meteor.position.y)) texture meteor_pixel_size meteor_pixel_size 45.0

