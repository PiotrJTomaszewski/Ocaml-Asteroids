open Sdl

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
