open Sdl

let pixel_for_surface ~surface ~rgb =
  let fmt = Surface.get_pixelformat_t surface in
  let pixel_format = Pixel.alloc_format fmt in
  let pixel = Pixel.map_RGB pixel_format rgb in
  Pixel.free_format pixel_format;
  (pixel)


let load_sprite renderer ~filename =
  let surface = Surface.load_bmp ~filename in
  (* transparent pixel from white background *)
  let rgb = (255, 255, 255) in
  let key = pixel_for_surface ~surface ~rgb in
  Surface.set_color_key surface ~enable:true ~key;
  let tex = Texture.create_from_surface renderer surface in
  Surface.free surface;
  (tex)