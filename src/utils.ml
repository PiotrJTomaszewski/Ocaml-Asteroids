open Sdl;;
open Common;;

let rec for_each func items = match items with
  | [] -> ();
  | hd :: tl -> func hd; for_each func tl;
;;


let speed_limit speed limit =
  if speed > 0. then
    min speed limit
  else
    max speed (-.limit)
;;

(*calculates new position also checks if edge has been reached and move to opposite edge*)
let update_position_with_warp position speed obj_pixel_size time_delta =
  let calculated_position_x = position.x +. (speed.x *. time_delta) in
  let calculated_position_y = position.y +. (speed.y *. time_delta) in
  let new_position_x =
    if calculated_position_x < (-. obj_pixel_size) then
      float_of_int(Constants.window_width)
    else if calculated_position_x > float_of_int(Constants.window_width) then
      0.
    else
      calculated_position_x
  in
  let new_position_y =
    if calculated_position_y < (-. obj_pixel_size) then
      float_of_int(Constants.window_height)
    else if calculated_position_y > float_of_int(Constants.window_height) then
      0.
    else
      calculated_position_y
  in
  {
    x = new_position_x;
    y = new_position_y
  }


let random_int min_val max_val =
  Random.int (max_val - min_val + 1) + min_val


let random_float min_val max_val =
  Random.float (max_val -. min_val) +. min_val
