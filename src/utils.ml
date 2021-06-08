open Sdl;;
open Common;;


let rec for_each func items = match items with
  | [] -> ();
  | hd :: tl -> func hd; for_each func tl;
;;

(* Clamps the newly calculated speed so it won't be greater than the speed limit *)
let speed_limit speed limit =
  if speed > 0. then
    min speed limit
  else
    max speed (-.limit)
;;

(* Calculates new position also checks if edge has been reached and move to opposing edge *)
let update_position_with_warp position speed obj_pixel_size time_delta =
  let calculated_position_x = position.x +. (speed.x *. time_delta) in
  let calculated_position_y = position.y +. (speed.y *. time_delta) in
  let obj_half_pixel_size = obj_pixel_size /. 2. in
  let new_position_x =
    if calculated_position_x < -.obj_half_pixel_size then
      float_of_int(Constants.window_width)
    else if calculated_position_x -. float_of_int(Constants.window_width) > obj_half_pixel_size then
      0.
    else
      calculated_position_x
  in
  let new_position_y =
    if calculated_position_y < -.obj_half_pixel_size then
      float_of_int(Constants.window_height)
    else if calculated_position_y -. float_of_int(Constants.window_height) > obj_half_pixel_size then
      0.
    else
      calculated_position_y
  in
  {
    x = new_position_x;
    y = new_position_y
  }


(* Generate a random integer in range min_val (inclusive) to max_val (inclusive) *)
let random_int min_val max_val =
  Random.int (max_val - min_val + 1) + min_val


(* Generate a random float in range min_val (inclusive) to max_val (inclusive) *)
let random_float min_val max_val =
  Random.float (max_val -. min_val) +. min_val
