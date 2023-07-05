open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)
let transform image ~radius =
  Image.mapi image ~f:(fun ~x ~y _pixel ->
    (* FOR SLICE: (max for x - radius, 0) (min for x + radius, image.width)
       (max for y - radius, 0) (min for y + radius, image.height) *)
    Image.mean_pixel
      (Image.slice
         image
         ~x_start:(Core.max (x - radius) 0)
         ~x_end:(Core.min (x + radius) (Image.width image))
         ~y_start:(Core.max (y - radius) 0)
         ~y_end:(Core.min (y + radius) (Image.height image))))
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
