open Core

let horizontal_gradient image x y : int =
  let x_min = if x - 1 < 0 then 1 else x - 1 in
  let x_max = if x + 1 >= Image.width image then x - 1 else x + 1 in
  let y_min = if y - 1 < 0 then 1 else y - 1 in
  let y_max = if y + 1 >= Image.height image then y - 1 else y + 1 in
  (-1 * Pixel.red (Image.get image ~x:x_min ~y:y_min))
  + Pixel.red (Image.get image ~x:x_max ~y:y_min)
  + (-2 * Pixel.red (Image.get image ~x:x_min ~y))
  + (2 * Pixel.red (Image.get image ~x:x_max ~y))
  + (-1 * Pixel.red (Image.get image ~x:x_min ~y:y_max))
  + Pixel.red (Image.get image ~x:x_max ~y:y_max)
;;

let vertical_gradient image x y : int =
  let x_min = if x - 1 < 0 then 1 else x - 1 in
  let x_max = if x + 1 >= Image.width image then x - 1 else x + 1 in
  let y_min = if y - 1 < 0 then 1 else y - 1 in
  let y_max = if y + 1 >= Image.height image then y - 1 else y + 1 in
  (-1 * Pixel.red (Image.get image ~x:x_min ~y:y_min))
  + (-1 * Pixel.red (Image.get image ~x:x_max ~y:y_min))
  + (-2 * Pixel.red (Image.get image ~x ~y:y_min))
  + (2 * Pixel.red (Image.get image ~x ~y:y_max))
  + Pixel.red (Image.get image ~x:x_min ~y:y_max)
  + Pixel.red (Image.get image ~x:x_max ~y:y_max)
;;

let gradient image x y : float =
  let grad_x = Float.of_int (horizontal_gradient image x y) in
  let grad_y = Float.of_int (vertical_gradient image x y) in
  Core.sqrt (Core.Float.square grad_x +. Core.Float.square grad_y)
;;

let transform image threshold radius : Image.t =
  let edit = Grayscale.transform image |> Blur.transform ~radius in
  let new_image = Image.copy edit in
  let max = Float.of_int (Image.max_val edit) in
  Image.mapi edit ~f:(fun ~x ~y _ ->
    if Float.compare
         (gradient edit x y /. max)
         (Float.of_int threshold /. 100.0)
       > 0
    then (
      Image.set new_image ~x ~y (Pixel.of_int (Int.of_float max));
      print_s [%message "setting to max"])
    else (
      print_s [%message "setting to zero"];
      Image.set new_image ~x ~y Pixel.zero);
    Image.get new_image ~x ~y)
;;

let command =
  Command.basic
    ~summary:"Detect Edges of an image"
    [%map_open.Command
      let threshold =
        flag
          "threshold"
          (required Command.Param.int)
          ~doc:"INT the integer representing the threshold %"
      and radius =
        flag "radius" (required Command.Param.int) ~doc:"INT the blur radius"
      and filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image threshold radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_edges.ppm")]
;;
