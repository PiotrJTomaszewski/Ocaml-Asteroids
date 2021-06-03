open Common;;

type bullet_t = {
  position: Common.vectorf2d_t;
  speed: Common.vectorf2d_t;
  angle: float;
};;


let spawn_bullet position angle =
  let theta = (angle -. 90.) *. 0.0174532 in
  let speed_x = cos theta *. Constants.bullet_speed in
  let speed_y = sin theta *. Constants.bullet_speed in

 {
   position = {
     x = position.x;
     y = position.y
   };
   speed = {
     x = speed_x;
     y =  speed_y 
   };
   angle = angle
 }


let update_bullet_position bullet time_delta =
  let new_position_x = bullet.position.x +. (bullet.speed.x *. time_delta) in
  let new_position_y = bullet.position.y +. (bullet.speed.y *. time_delta) in
  (* Remove bullets that have left the screen *)
  if 
    new_position_x >= 0. && float_of_int Constants.window_width -. new_position_x >= 0. &&
    new_position_y >= 0. && float_of_int Constants.window_height -. new_position_y >= 0.
  then
    Some({
      bullet with
        position = {
          x = new_position_x;
          y = new_position_y;
        };
    })
  else
    None


let render_bullet renderer texture bullet =
  Show.put_texture renderer  (int_of_float bullet.position.x, int_of_float(bullet.position.y)) texture 16 32 bullet.angle
