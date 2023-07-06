open Core

let cut num bit : int =
  let power = Core.Int.pow 2 bit in
  if num >= power then num - power else num
;;

let keep_last_two num : int =
  let rec helper n bit : int =
    match bit with 1 -> n | _ -> helper (cut n bit) (bit - 1)
  in
  helper num 8
;;

let decrypt num : int =
  let x = keep_last_two num in
  x lsl 6
;;

let transform image =
  Image.map image ~f:(fun (r, g, b) ->
    Pixel.create (decrypt r) (decrypt g) (decrypt b))
;;

let command =
  Command.basic
    ~summary:"Convert an image to steganography"
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
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_steganography.ppm")]
;;
