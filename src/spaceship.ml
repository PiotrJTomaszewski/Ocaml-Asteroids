open Sdl

type spaceship_t = {
  position: Common.vectorf2d_t;
  speed: Common.vectorf2d_t;
}


let update_spaceship_position spaceship time_delta = {
  spaceship with
    position = {
      x = spaceship.position.x +. (spaceship.speed.x *. float_of_int(time_delta));
      y = spaceship.position.y +. (spaceship.speed.y *. float_of_int(time_delta));
    }
}


let render_spaceship renderer texture spaceship =
  Show.put_texture renderer  (int_of_float spaceship.position.x, int_of_float(spaceship.position.y)) texture 64 64 45.0
