type vectorf2d_t = {
  x: float;
  y: float;
}


type action =
  | Up
  | Down
  | Left
  | Right
  | Fire
;;


let pair_float_of_vectorf2d_t fv = (fv.x,fv.y)


let pair_int_of_vectorf2d_t fv = (int_of_float fv.x,int_of_float fv.y)
