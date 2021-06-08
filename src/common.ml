(* 2 dimentional vector with floating point coordinates *)
type vectorf2d_t = {
  x: float;
  y: float;
}


(* Spaceship action types *)
type action =
  | Up
  | Down
  | Left
  | Right
  | Fire
;;

(* Converts vector from a record to a tuple *)
let pair_float_of_vectorf2d_t fv = (fv.x,fv.y)

