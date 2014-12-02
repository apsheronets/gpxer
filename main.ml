
open Printf

let (|>) f g = g f (* default in OCaml 4.01 *)
let (@@) f x = f x

let rec exude p = function
  | [] -> raise Not_found
  | x :: l -> (match p x with
      | Some y -> y
      | None -> exude p l)

type latlon = (float * float)

type trkpt = latlon (* we don't need (yet) another crap *)
type trkseg = latlon list
type trk = trkseg list
type gpx = trk list

let gpx_of_channel channel : gpx =
  let i = Xmlm.make_input (`Channel channel) in
  let module Xml = Xmlm_shit in
  let r =
    Xml.find_tag "gpx" i @@
      fun _ i -> Xml.input_list_ignore_other i @@
        Xml.expect_tag "trk" @@
          fun _ i -> Xml.input_list_ignore_other i @@
            Xml.expect_tag "trkseg" @@
              fun _ i -> Xml.input_list_ignore_other i @@
                Xml.expect_tag "trkpt" @@
                  fun ((_, _), attrs) i ->
                    let lat =
                      exude
                        (fun ((_, name), value) ->
                          if name = "lat" then Some value else None)
                        attrs
                    and lon =
                      exude
                        (fun ((_, name), value) ->
                          if name = "lon" then Some value else None)
                        attrs in
                    Xml.close_tag i;
                    Xml.Parsed (float_of_string lat, float_of_string lon)
  in
  match r with
  | Xml.Parsed r -> r
  | Xml.Failed -> assert false (* bad gpx *)

let print_gpx =
  List.iter
    (List.iter
      (List.iter
        (fun (lat, lon) -> printf "(%f, %f)\n" lat lon)))

let tile_width = 256
let tile_height = 256

let calc_border init f (gpx:gpx) : float =
  let r = ref init in
  List.iter
    (List.iter
      (List.iter
        (fun latlon -> f latlon r)))
    gpx;
  if !r = init
  then assert false
  else !r

let min_lat =
  calc_border ( 666.) (fun (lat, _) r -> if !r > lat then r := lat)

let min_lon =
  calc_border ( 666.) (fun (_, lon) r -> if !r > lon then r := lon)

let max_lat =
  calc_border (-666.) (fun (lat, _) r -> if !r < lat then r := lat)

let max_lon =
  calc_border (-666.) (fun (_, lon) r -> if !r < lon then r := lon)

(* some math *)
let pi = 3.14159265358979323846
let log2 x = log x /. log 2.0

let calc_zoom lat_min lat_max lon_min lon_max width height =
  let zoom_to_fit_width = int_of_float (floor (log2(float_of_int (360 * width) /. (float_of_int tile_width *. (lon_max -. lon_min))))) in
  printf "calculated maximum zoom to fit width:  %d\n" zoom_to_fit_width;
  let zoom_to_fit_height =
    int_of_float @@
    log2 ((pi *. float_of_int height) /. ((float_of_int tile_height) *. (log (tan (lat_max *. (pi /. 180.)) +. (1. /. cos (lat_max *. (pi /. 180.)))) -. log (tan (lat_min *. (pi /. 180.)) +. (1. /. cos (lat_min *. (pi /. 180.))))))) +. 1.0 in
  printf "calculated maximum zoom to fit height: %d\n" zoom_to_fit_height;
  let zoom =
    if zoom_to_fit_width < zoom_to_fit_height
    then zoom_to_fit_width
    else zoom_to_fit_height in
  printf "calculated minimum zoom: %d\n" zoom;
  if zoom > 18
  then (* in case we got one meter GPX *)
    (printf "zoom is too high, choose 18\n";
    18)
  else if zoom < 0
  then
    (printf "zoom is too low, choose 0";
    0)
  else
    zoom

let x_tile_of_lon zoom lon =
  int_of_float (((lon +. 180.) /. 360.) *. (2. ** float_of_int zoom))

(* on global map *)
let x_pixel_of_lon zoom lon =
  ((lon +. 180.) /. 360.) *. (2. ** float_of_int zoom) *. float_of_int tile_width

let y_tile_of_lat zoom lat =
  int_of_float ((1.0 -. ((log (tan (lat *. (pi /. 180.)) +. (1. /. cos (lat *. (pi /. 180.))))) /. pi)) *. (2. ** float_of_int (zoom - 1)))

(* on global map *)
let y_pixel_of_lat zoom lat =
  (1.0 -. ((log (tan (lat *. (pi /. 180.)) +. (1. /. cos (lat *. (pi /. 180.))))) /. pi)) *. (2. ** float_of_int (zoom - 1)) *. float_of_int tile_height

let gpx_height_in_pixels zoom min_lat max_lat =
  let top    = y_pixel_of_lat zoom max_lat in
  let bottom = y_pixel_of_lat zoom min_lat in
  bottom -. top

let gpx_width_in_pixels zoom min_lon max_lon =
  let min_width = x_pixel_of_lon zoom min_lon in
  let max_width = x_pixel_of_lon zoom max_lon in
  max_width -. min_width

let canvas_top canvas_height gpx_height zoom max_lat =
  let vertical_padding = (float_of_int canvas_height -. gpx_height) /. 2. in
  y_pixel_of_lat zoom max_lat -. vertical_padding

let canvas_bottom canvas_height gpx_height zoom min_lat =
  let vertical_padding = (float_of_int canvas_height -. gpx_height) /. 2. in
  y_pixel_of_lat zoom min_lat +. vertical_padding

let canvas_left canvas_width gpx_width zoom min_lon =
  let horizontal_padding = (float_of_int canvas_width -. gpx_width) /. 2. in
  x_pixel_of_lon zoom min_lon -. horizontal_padding

let canvas_right canvas_width gpx_width zoom max_lon =
  let horizontal_padding = (float_of_int canvas_width -. gpx_width) /. 2. in
  x_pixel_of_lon zoom max_lon +. horizontal_padding

let top_tile zoom canvas_top =
  int_of_float (canvas_top /. float_of_int tile_height)

let bottom_tile zoom canvas_bottom =
  int_of_float (canvas_bottom /. float_of_int tile_height)

let left_tile zoom canvas_left =
  int_of_float (canvas_left /. float_of_int tile_width)

let right_tile zoom canvas_right =
  int_of_float (canvas_right /. float_of_int tile_width)

type tile = {
  x: int;
  y: int;
  file: string;
}

open Arg

let () =
  let help =
    "gpxer - GPX to PNG tool\n" ^
    "usage: gpxer [OPTIONS] < /path/to/gpx/file.gpx\n" in
  let out = ref "out.png" in
  let canvas_height = ref 600 in
  let canvas_width = ref 800 in
  let osm_base_url = ref "http://{s}.tile.opencyclemap.org/landscape/{z}/{x}/{y}.png" in
  let horizontal_padding  = ref 0 in
  let compress = ref false in
  let source = ref "" in
  let l = [
    "-o", Set_string out, sprintf "FILE\twrite output to <file>; default is %S" !out;
    "-height", Set_int canvas_height, sprintf "\timage height; default is %d" !canvas_height;
    "-width", Set_int canvas_width, sprintf "\timage width; default is %d" !canvas_width;
    "-url", Set_string osm_base_url, sprintf "\t\tURL to download tiles from;\n\t\tdefault is %s" !osm_base_url;
    "-horizontal-padding",  Set_int horizontal_padding,  sprintf "\tCSS-like property for canvas; default is %d" !horizontal_padding;
    "-compress", Set compress, "\t\tebnable aggressive compresion"
  ] in
  Arg.parse l (fun a -> source := a) help;

  let canvas_height = !canvas_height in
  let canvas_width  = !canvas_width  in
  let out = !out in
  let osm_base_url = !osm_base_url in
  let horizontal_padding = !horizontal_padding in

  let basedir = Filename.dirname (Sys.executable_name) in
  let start_icon = basedir ^ "/../share/gpxer/pin-icon-start.png" in
  let end_icon = basedir ^ "/../share/gpxer/pin-icon-end.png" in
  let shadow = basedir ^ "/../share/gpxer/pin-shadow.png" in

  let tmp_dir =
    Random.self_init ();
    let rand = Random.int 1073741823 in
    sprintf "/tmp/gpxer-%d" rand in

  let die code =
    printf "removing temporary directory %s\n%!" tmp_dir;
    exit code in

  let command_or_die cmd =
    match Sys.command cmd with
    | 0 -> ()
    | code -> eprintf "%S failed with exit code %d\n%!" cmd code; die 1 in

  printf "parsing gpx file\n%!";
  let gpx = gpx_of_channel stdin in
  (*print_gpx gpx;*)
  let min_lat = min_lat gpx in
  let max_lat = max_lat gpx in
  let min_lon = min_lon gpx in
  let max_lon = max_lon gpx in
  let zoom =
    calc_zoom
      min_lat
      max_lat
      min_lon
      max_lon
      (canvas_width - horizontal_padding)
      canvas_height in
  let gpx_height = gpx_height_in_pixels zoom min_lat max_lat in
  printf "gpx height: %f\n" gpx_height;
  let gpx_width  = gpx_width_in_pixels zoom min_lon max_lon in
  printf "gpx width:  %f\n" gpx_width;
  let canvas_top = canvas_top canvas_height gpx_height zoom max_lat in
  printf "canvas top:    %f\n" canvas_top;
  let canvas_bottom = canvas_bottom canvas_height gpx_height zoom min_lat in
  printf "canvas bottom: %f\n" canvas_bottom;
  let canvas_left = canvas_left canvas_width gpx_width zoom min_lon in
  printf "canvas left:   %f\n" canvas_left;
  let canvas_right = canvas_right canvas_width gpx_width zoom max_lon in
  printf "canvas right:  %f\n" canvas_right;
  let top_tile = top_tile zoom canvas_top in
  let bottom_tile = bottom_tile zoom canvas_bottom in
  let left_tile = left_tile zoom canvas_left in
  let right_tile = right_tile zoom canvas_right in
  let osm_url zoom x y =
    let str = osm_base_url in
    let _, str = ExtLib.String.replace ~str ~sub:"{s}" ~by:"a" (* FIXME in the future *) in
    let _, str = ExtLib.String.replace ~str ~sub:"{z}" ~by:(string_of_int zoom) in
    let _, str = ExtLib.String.replace ~str ~sub:"{x}" ~by:(string_of_int x   ) in
    let _, str = ExtLib.String.replace ~str ~sub:"{y}" ~by:(string_of_int y   ) in
    str in
  let tiles =
    let rec xloop x acc =
      let rec yloop y acc =
        if y > bottom_tile
        then acc
        else
          let dir = sprintf "%s/%d/%d" tmp_dir zoom x in
          let file = sprintf "%s/%d.png" dir y in
          let url = osm_url zoom x y in
          command_or_die (sprintf "mkdir -p %s" dir);
          printf "downloading tile %s\n%!" url;
          command_or_die (sprintf "curl -s %s -o %s" url file);
          let tile = { file; x; y } in
          yloop (succ y) (tile :: acc) in
      if x > right_tile
      then acc
      else
        let column = yloop top_tile [] in
        xloop (succ x) (List.rev_append column acc) in
    xloop left_tile [] in
  let image = Magick.get_canvas ~width:canvas_width ~height:canvas_height ~color:"#000000" in
  tiles |> List.iter (fun tile ->
    let tile_image = Magick.read_image ~filename:tile.file in
    let x = (tile.x * tile_width)  - int_of_float (floor canvas_left) in
    let y = (tile.y * tile_height) - int_of_float (floor canvas_top ) in
    Magick.Imper.composite_image image tile_image ~compose:Magick.Over ~x ~y ());
  printf "removing temporary directory %s\n%!" tmp_dir;
  command_or_die (sprintf "rm -r %s" tmp_dir);
  let start_icon  = Magick.read_image ~filename:start_icon in
  let end_icon    = Magick.read_image ~filename:end_icon   in
  let shadow      = Magick.read_image ~filename:shadow     in
  let draw_icon icon (lat, lon) =
    let x = int_of_float @@ x_pixel_of_lon zoom lon -. canvas_left in
    let y = int_of_float @@ y_pixel_of_lat zoom lat -. canvas_top in
    let icon_width  = Magick.get_image_width  icon in
    let icon_height = Magick.get_image_height icon in
    let x = x - (icon_width / 2) in
    let y = y - icon_height + 5 (* a magic five *) in
    Magick.Imper.composite_image image icon ~compose:Magick.Over ~x ~y ();
    Magick.Imper.composite_image image shadow ~compose:Magick.Over ~x ~y () in
  gpx |>
    List.iter @@
      List.iter
        (fun trkseg ->
          let rec travel (lat0, lon0) rest =
            match rest with
            | [] -> (lat0, lon0);
            | (lat1, lon1)::t ->
                let x0 = int_of_float @@ x_pixel_of_lon zoom lon0 -. canvas_left in
                let x1 = int_of_float @@ x_pixel_of_lon zoom lon1 -. canvas_left in
                let y0 = int_of_float @@ y_pixel_of_lat zoom lat0 -. canvas_top in
                let y1 = int_of_float @@ y_pixel_of_lat zoom lat1 -. canvas_top in
                Magick.Imper.draw_line
                  image
                  ~x0
                  ~x1
                  ~y0
                  ~y1
                  ~fill_color:(Magick.Imper.color_of_string "#4e5fcd")
                  ~stroke_color:(Magick.Imper.color_of_string "#4e5fcd")
                  ~stroke_width:4.0
                  ~stroke_antialias:Magick.MagickTrue
                  ~line_cap:Magick.Imper.RoundCap
                  ();
                travel (lat1, lon1) t in
          match trkseg with
          | first::rest ->
              let last = travel first rest in
              draw_icon start_icon first;
              draw_icon end_icon last;
              ()
          | _ -> ()); (* FIXME in the future*)
  printf "writing to %s\n%!" out;

  if !compress then begin
    (* actually I have no idea what I'm doing *)
    (* but this works *)
    Magick.Imper.set_image_type image Magick.Palette;
    Magick.Imper.set_image_colors image 0;
    Magick.Imper.set_compression_quality image 0;
    Magick.Imper.strip_image image;
  end;

  Magick.write_image image out;
  ()

