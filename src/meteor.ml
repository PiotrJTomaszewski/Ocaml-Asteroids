type meteor_t = {
  position: Common.vectorf2d_t;
  speed: Common.vectorf2d_t;
  size: int;
}

(* Generates a new meteor *)
let spawn_meteor () =
  let size = Utils.random_int Constants.min_meteor_init_size Constants.max_meteor_init_size in
  let max_x = float_of_int (Constants.window_width - (size * Constants.meteor_size_scale)) in
  let max_y = float_of_int (Constants.window_height - (size * Constants.meteor_size_scale)) in
  {
    (* Meteors can spawn only on the edges of the screen *)
    position = (
      match Random.int 4 with
      | 0 -> {x = 0.; y = Random.float max_y};
      | 1 -> {x = max_x; y = Random.float max_y};
      | 2 -> {x = Random.float max_x; y = 0.};
      | _ -> {x = Random.float max_x; y = max_y};
    );
    speed = {
      x = Utils.random_float Constants.min_meteor_speed Constants.max_meteor_speed;
      y = Utils.random_float Constants.min_meteor_speed Constants.max_meteor_speed
    };
    size = size;
  }


(* Calculate meteor position on a new frame *)
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


(* When a meteor is hit with a bullet it's replaced with a few smaller meteors
  returns a tuple: a list of new meteors and a number of points received for destroying the meteor
 *)
let split_meteor_on_collision meteor is_collision =
  let rec _inner position size counter =
    (* Splitted meteors can skew slightly into a new direction *)
    match counter with
      | 0 -> []
      | _ -> (_inner position size (counter-1))@[{
        position = position;
        speed = {
          x = Utils.random_float Constants.min_meteor_speed Constants.max_meteor_speed;
          y = Utils.random_float Constants.min_meteor_speed Constants.max_meteor_speed
        };
        size = size;
        }]
  in
  if is_collision then
    if meteor.size > 1 then
      _inner meteor.position (meteor.size - 1) (Utils.random_int Constants.min_meteor_split Constants.max_meteor_split), meteor.size * Constants.meteor_score
    else
      [], Constants.meteor_score
   else
    [meteor], 0


let render_meteor renderer texture meteor =
  let meteor_pixel_size = meteor.size * Constants.meteor_size_scale in
  Show.put_texture renderer  (int_of_float meteor.position.x, int_of_float(meteor.position.y)) texture meteor_pixel_size meteor_pixel_size 45.0


let add_meteor_if_exists meteor meteor_list =
  if (meteor.size>0) then meteor::meteor_list else meteor_list
;;
