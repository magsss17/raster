open Core

let invert_if_needed color max threshold =
  if Float.compare
       (Float.of_int color /. max)
       (Float.of_int threshold /. 100.0)
     > 0
  then Int.of_float max - color
  else color
;;

let transform image threshold : Image.t =
  let max = Float.of_int (Image.max_val image) in
  Image.map image ~f:(fun (r, g, b) ->
    Pixel.create
      (invert_if_needed r max threshold)
      (invert_if_needed g max threshold)
      (invert_if_needed b max threshold))
;;

let command =
  Command.basic
    ~summary:"Solarize an image"
    [%map_open.Command
      let threshold =
        flag
          "threshold"
          (required Command.Param.int)
          ~doc:"INT the integer representing the threshold %"
      and filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image threshold in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_solarized.ppm")]
;;
