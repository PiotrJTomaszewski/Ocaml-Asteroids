type meteor_t = {
  position: Common.vectorf2d_t;
  speed: Common.vectorf2d_t;
  (* color: Sdlpixel.rgb; *)
  size: int;
}


let spawn_meteor () =
  (* Bound parameter in Random is exclusive hence +1 everywhere *)
  let size = Utils.random_int 1 Constants.max_meteor_init_size in
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
      x = Utils.random_float Constants.min_meteor_speed Constants.max_meteor_speed;
      y = Utils.random_float Constants.min_meteor_speed Constants.max_meteor_speed
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


let rec _split_meteor_loop position  size counter =
  (* Splitted meteors can skew slightly into a new direction *)
  match counter with
    | 0 -> []
    | _ -> (_split_meteor_loop position size (counter-1))@[{
      position = position;
      speed = {
        x = Utils.random_float Constants.min_meteor_speed Constants.max_meteor_speed;
        y = Utils.random_float Constants.min_meteor_speed Constants.max_meteor_speed
      };
      size = size;
      }]


let split_meteor_on_collision meteor is_collision =
  if is_collision then
    if meteor.size > 1 then
      _split_meteor_loop meteor.position (meteor.size - 1) (Utils.random_int Constants.min_meteor_split Constants.max_meteor_split)
    else
      []
   else
    [meteor]


let render_meteor renderer texture meteor =
  let meteor_pixel_size = meteor.size * Constants.meteor_size_scale in
  Show.put_texture renderer  (int_of_float meteor.position.x, int_of_float(meteor.position.y)) texture meteor_pixel_size meteor_pixel_size 45.0

  let add_meteor_if_exists meteor meteor_list =
    if (meteor.size>0) then meteor::meteor_list else meteor_list
  ;;