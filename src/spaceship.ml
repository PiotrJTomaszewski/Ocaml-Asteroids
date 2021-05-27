open Sdl


type spaceship_t = {
  position: Common.vectorf2d_t;
  speed: Common.vectorf2d_t;
}


let update_spaceship_position spaceship time_delta = {
  spaceship with
    position = {
      x = spaceship.position.x +. (spaceship.speed.x *. time_delta);
      y = spaceship.position.y +. (spaceship.speed.y *. time_delta);
    }
}


let render_spaceship renderer spaceship = ()