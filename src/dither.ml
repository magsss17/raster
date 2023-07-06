open Core

(* This should look familiar by now! *)
let transform image =
  let gray = Grayscale.transform image in
  let max = Image.max_val gray in
  let height = Image.height gray in
  let width = Image.width gray in
  let e_error err =
    Int.of_float (Float.of_int 7 /. Float.of_int 16 *. Float.of_int err)
  in
  let sw_error err =
    Int.of_float (Float.of_int 3 /. Float.of_int 16 *. Float.of_int err)
  in
  let s_error err =
    Int.of_float (Float.of_int 5 /. Float.of_int 16 *. Float.of_int err)
  in
  let se_error err =
    Int.of_float (Float.of_int 1 /. Float.of_int 16 *. Float.of_int err)
  in
  let _ =
    Image.mapi gray ~f:(fun ~x ~y pixel ->
      let r = Pixel.red pixel in
      print_s [%message (r : int)];
      let err =
        if Float.compare (Float.of_int r /. Float.of_int max) 0.5 > 0
        then (
          print_s [%message "then"];
          Image.set gray ~x ~y (Pixel.of_int max);
          r - max)
        else (
          print_s [%message "else"];
          Image.set gray ~x ~y Pixel.zero;
          r)
      in
      if x + 1 < width
      then (
        let og = Pixel.red (Image.get gray ~x:(x + 1) ~y) in
        Image.set gray ~x:(x + 1) ~y (Pixel.of_int (og + e_error err)));
      if x - 1 >= 0 && y + 1 < height
      then (
        let og = Pixel.red (Image.get gray ~x:(x - 1) ~y:(y + 1)) in
        Image.set
          gray
          ~x:(x - 1)
          ~y:(y + 1)
          (Pixel.of_int (og + sw_error err)));
      if y + 1 < height
      then (
        let og = Pixel.red (Image.get gray ~x ~y:(y + 1)) in
        Image.set gray ~x ~y:(y + 1) (Pixel.of_int (og + s_error err)));
      if x + 1 < width && y + 1 < height
      then (
        let og = Pixel.red (Image.get gray ~x:(x + 1) ~y:(y + 1)) in
        Image.set
          gray
          ~x:(x + 1)
          ~y:(y + 1)
          (Pixel.of_int (og + se_error err)));
      Image.get gray ~x ~y)
  in
  gray
;;

let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
