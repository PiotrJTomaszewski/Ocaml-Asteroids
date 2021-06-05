open Sdl;;
module TexMap = Map.Make(String);;

let red = (255,0,0)
let black = (0,0,0)
let alpha = 255

(*draws a rectangle*)
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
;;

(*read a texture from a file in given localization*)
let get_tex renderer localization =
  let rw = RWops.from_file localization "rb" in
  let img = Sdlimage.load_png_rw rw in
  RWops.free rw;
  let tex = Texture.create_from_surface renderer img in
  Surface.free img;
  tex


(*
UNSAFE!! because it must keep length(list_names) == length(list_addresses)
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


(*drawing a texture in given coodinates 
  render -> renderer of the window/screen
  (x,y) -> center of rectangle with 
  texture -> texture to put on
  l1 -> width
  l2 -> length
  angle_gr -> angle in grades <0;360>
  *)
let put_texture render (x,y) texture l1 l2 angle_gr=
    let rect = Rect.make ((x-(l1/2)),(y-(l2/2))) (l1,l2) in
    Render.copyEx render ~texture:texture ~dst_rect:rect () ~angle:angle_gr;
;;

(*drawing surface in given position and dimensions*)
let draw_rect_surf renderer surf pos col (w,h)=
    Render.set_draw_color renderer ~rgb:col ~a:alpha;
    let t = Texture.create_from_surface renderer surf in
    put_texture renderer pos t  w h 0.0;
    Texture.destroy t
  ;;
  (*rendering game over text*)
  let game_over_text renderer font =
    let game_over_surf = Sdlttf.render_text_solid font "Game Over" {r = 255; g=255; b=255; a=255;} in
    draw_rect_surf renderer game_over_surf ((Constants.window_width/2),(Constants.window_height/2 - 50)) black (10 * 18 + 70,40);
    Surface.free game_over_surf;
  let restart_surf = Sdlttf.render_text_solid font "Press R" {r = 255; g=255; b=255; a=255;} in
    draw_rect_surf renderer restart_surf ((Constants.window_width/2),(Constants.window_height/2 + 50)) black (8 * 18 + 70,40);
    Surface.free restart_surf