open Sdl;;
module TexMap = Map.Make(String);;

let red = (255,0,0)
let black = (0,0,0)
let alpha = 255

let fill_rect renderer (x, y) (w,h) =
  let rect = Rect.make4 ~x:(x-w/2) ~y:(y-h/2) ~w:w ~h:h in
  Render.fill_rect renderer rect;
;;

(*
Draws a rectangle
 renderer ->
pos -> center point of drawn rectangle
col -> color (int * int * int)
dim -> width X height (int *int)
*)
let draw_rect renderer pos col dim=
  Render.set_draw_color renderer ~rgb:col ~a:alpha;
  fill_rect renderer pos dim;
  Render.render_present renderer;
;;

let get_tex renderer str =
  let rw = RWops.from_file str "rb" in
  let img = Sdlimage.load_png_rw rw in
  RWops.free rw;
  Texture.create_from_surface renderer img

(*
UNSAFE!! because it must  length(list_names) == length(list_addresses)
render -> renderer of the window/screen
list_names -> list of names given for textures to use in val TexMap.get
list_addresses -> list of locations of PNG (!!) files
*)
  let load_tex renderer list_names list_addresses=
  let rec map_all ln la map =
    match la with
      [] -> map
    | hd::tl -> map_all (List.tl ln) tl (TexMap.add (List.hd ln) (get_tex renderer hd) map)
  in
    map_all list_names list_addresses TexMap.empty


(* render -> renderer of the window/screen
  (x,y) -> center of rectangle with 
  texture -> texture to put on
  l1 -> width
  l2 -> length
  angle_gr -> angle in grades <0;360>
  *)
let put_texture render (x,y) texture l1 l2 angle_gr=
    let rect = Rect.make ((x-(l1/2)),(y-(l2/2))) (l1,l2) in
    Render.copyEx render ~texture:texture ~dst_rect:rect () ~angle:angle_gr;
    (* Render.render_present render;; *) (* We only have to call this function once every frame. Calling it after every drawn item is unnecessary *)
