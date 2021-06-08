(* Window config *)
let window_height = 600;;

let window_width = 800;;


(* Meteor config *)
let min_meteor_speed = 0.05;; (* Min meteor speed in pixels/millisecond *)

let max_meteor_speed = 0.15;; (* Max meteor speed in pixels/millisecond *)

let min_meteor_init_count = 2;;

let max_metor_init_count = 4;;

let min_meteor_split = 1;;

let max_meteor_split = 3;;

let min_meteor_init_size = 1;; (* Min initial meteor size level *)

let max_meteor_init_size = 5;; (* Max initial meteor size level *)

let meteor_size_scale = 20;; (* Multiplier of the current size level to get the size in pixels *)

let meteor_score = 100;; (* Multiplier of the current size level to get the number of points earned for destroying *)


(* Spaceship config *)
let spaceship_speed_delta = 0.1;; (* Spaceship speed is increased/decreased by this amount on each input *)

let spaceship_max_speed = 0.3;; (* Max spaceship speed in pixels/millisecond  *)

let spaceship_size = 64;; (* Spaceship size in pixels *)


(* Bullet config *)
let bullet_speed = 0.6;; (* Laser bullet speed in pixels/millisecond *)
