open Core

let get_val pixel ~max : float =
  let r, g, b = pixel in
  Float.of_int (r + g + b) /. max
;;

let mse ~regionA ~regionB ~width ~height ~max : float =
  let sum =
    Image.foldi regionA ~init:0.0 ~f:(fun ~x ~y acc pixel ->
      Float.square
        (get_val pixel ~max -. get_val (Image.get regionB ~x ~y) ~max)
      +. acc)
  in
  1.0 /. Float.of_int (width * height) *. sum
;;

let mosaic_generator image ~width ~height ~max : Image.t =
  let image_width = Image.width image in
  let image_height = Image.height image in
  (* pick a random region *)
  let x_start = Random.int (image_width - width) in
  let y_start = Random.int (image_height - height) in
  let rand =
    Image.slice
      ~x_start
      ~y_start
      ~x_end:(x_start + width)
      ~y_end:(y_start + height)
      image
  in
  (* divide the picture into regions of width x height *)
  let num_x = image_width / width in
  let num_y = image_height / width in
  let regions =
    List.cartesian_product
      (List.init num_x ~f:(fun x -> x * width))
      (List.init num_y ~f:(fun y -> y * height))
  in
  (* find the region with most similar pixels and return top left coords *)
  let swap_x, swap_y =
    match
      regions
      |> List.map ~f:(fun ((x, y) as pos) ->
           ( mse
               ~regionA:rand
               ~regionB:
                 (Image.slice
                    image
                    ~x_start:x
                    ~y_start:y
                    ~x_end:(x + width)
                    ~y_end:(y + height))
               ~width
               ~height
               ~max
           , pos ))
      |> List.min_elt ~compare:(fun (v1, _) (v2, _) -> Float.compare v1 v2)
    with
    | Some (_, (x, y)) -> x, y
    | None -> 0, 0
  in
  (* swap pixels *)
  let temp_slice =
    Image.slice
      image
      ~x_start:swap_x
      ~y_start:swap_y
      ~x_end:(swap_x + width)
      ~y_end:(swap_y + height)
  in
  (* switch region a to region b *)
  let _ =
    Image.mapi temp_slice ~f:(fun ~x ~y pixel ->
      Image.set image ~x:(x + x_start) ~y:(y + y_start) pixel;
      pixel)
  in
  (* switch region b to region a *)
  let _ =
    Image.mapi rand ~f:(fun ~x ~y pixel ->
      Image.set image ~x:(x + swap_x) ~y:(y + swap_y) pixel;
      pixel)
  in
  image
;;

(* let mse, image = List.fold regions ~init:(0, 0) ~f:(fun (x, y) acc -> let
   mse = mse ~rand ~regionB:(Image.slice image ~x_start:x ~y_start:y
   ~x_end:(x + width) ~y_end:(y + height)) ~width ~height ~max in Float.min
   mse acc) *)

let transform image ~width ~height ~moves : Image.t =
  let max = Float.of_int (Image.max_val image) in
  let rec helper times image' =
    match times with
    | 0 -> image'
    | _ -> helper (times - 1) (mosaic_generator image ~width ~height ~max)
  in
  helper moves image
;;

let command =
  Command.basic
    ~summary:"Solarize an image"
    [%map_open.Command
      let moves = flag "moves" (required Command.Param.int) ~doc:"INT moves"
      and filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and width = flag "width" (required Command.Param.int) ~doc:"INT width"
      and height =
        flag "height" (required Command.Param.int) ~doc:"INT height"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~moves ~width ~height in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_mosaic.ppm")]
;;
