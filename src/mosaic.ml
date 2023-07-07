open Core

let get_val pixel ~max : float = 
  let r, g, b = pixel in
  (Float.of_int (r + g + b)) /. max
;;

let mse ~regionA ~regionB ~width ~height ~max : float =
  let sum = Image.foldi regionA ~init:0.0 ~f:(fun ~x ~y acc pixel -> 
    Float.square ((get_val pixel ~max) -. (get_val (Image.get regionB ~x ~y) ~max)) +. acc) in
  1.0 /. (Float.of_int (width * height)) *. sum
;;

let mosaic_generator image ~width ~height ~max : Image.t = 
  let image_width = Image.width image in
  let image_height = Image.height image in
  (* pick a random region *)
  let x_start = Random.int (image_width - width) in
  let y_start = Random.int (image_height - height) in
  let rand = Image.slice ~x_start ~y_start ~x_end:(x_start + width) ~y_end:(y_start + height) image in

  (* divide the picture into regions of width x height *)
  let num_x = image_width / width in
  let num_y = image_height / width in
  let num_regions = num_x * num_y in





  (* find the region with most similar pixels and swap *)
;;


let transform image ~width ~height ~moves : Image.t = 
  let max = Float.of_int (Image.max_val image) in
  let rec helper times =
    match times with 
    | 0 -> image
    | _ -> mosaic_generator image ~width ~height ~max
  in
  helper moves
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
