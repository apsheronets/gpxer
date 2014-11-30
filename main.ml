
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
        (fun (lat, lon) -> Printf.printf "(%f, %f)\n" lat lon)))

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

let calc_zoom lat_min lat_max lon_min lon_max width height =
  let zoom_to_fit_width = int_of_float (floor (Math.log2(float_of_int (360 * width) /. (float_of_int tile_width *. (lon_max -. lon_min))))) in
  Printf.printf "Calculated maximum zoom to fit width %d\n" zoom_to_fit_width;
  let zoom_to_fit_height = int_of_float (floor (Math.log2 (float_of_int height /. (float_of_int tile_height *. (log (tan (lat_max *. (Constant.pi /. 180.0)) +. (1.0 /. cos (lat_max *. (Constant.pi /. 180.0)))) -. log (tan (lat_min *. (Constant.pi /. 180.0)) +. (1.0 /. cos(lat_min *. (Constant.pi /. 180.0))))) /. Constant.pi))) +. 1.) in
  Printf.printf "Calculated maximum zoom to fit height %d\n" zoom_to_fit_height;
  let zoom =
    if zoom_to_fit_width < zoom_to_fit_height
    then zoom_to_fit_width
    else zoom_to_fit_height in
  Printf.printf "Calculated minimum zoom %d\n" zoom;
  if zoom > 18
  then (* in case we got one meter GPX *)
    (Printf.printf "Zoom is too high, choose 18\n";
    18)
  else if zoom < 0
  then
    (Printf.printf "Zoom is too low, choose 0";
    0)
  else
    zoom

let x_tile_of_lon zoom lon =
  int_of_float (((lon +. 180.) /. 360.) *. (2. ** float_of_int zoom))

(* on global map *)
let x_pixel_of_lon zoom lon =
  ((lon +. 180.) /. 360.) *. (2. ** float_of_int zoom) *. float_of_int tile_width

let y_tile_of_lat zoom lat =
  int_of_float ((1.0 -. ((log (tan (lat *. (Constant.pi /. 180.)) +. (1. /. cos (lat *. (Constant.pi /. 180.))))) /. Constant.pi)) *. (2. ** float_of_int (zoom - 1)))

(* on global map *)
let y_pixel_of_lat zoom lat =
  (1.0 -. ((log (tan (lat *. (Constant.pi /. 180.)) +. (1. /. cos (lat *. (Constant.pi /. 180.))))) /. Constant.pi)) *. (2. ** float_of_int (zoom - 1)) *. float_of_int tile_height

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

let () =
  let gpx = gpx_of_channel stdin in
  print_gpx gpx;
  let canvas_height = 350 in
  let canvas_width  = 350 in
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
      canvas_height
      canvas_width in
  let gpx_height = gpx_height_in_pixels zoom min_lat max_lat in
  Printf.printf "gpx height: %f\n" gpx_height;
  let gpx_width  = gpx_width_in_pixels zoom min_lon max_lon in
  Printf.printf "gpx width:  %f\n" gpx_width;
  let canvas_top = canvas_top canvas_height gpx_height zoom max_lat in
  Printf.printf "canvas top:    %f\n" canvas_top;
  let canvas_bottom = canvas_bottom canvas_height gpx_height zoom min_lat in
  Printf.printf "canvas bottom: %f\n" canvas_bottom;
  let canvas_left = canvas_left canvas_width gpx_width zoom min_lon in
  Printf.printf "canvas left:   %f\n" canvas_left;
  let canvas_right = canvas_right canvas_width gpx_width zoom max_lon in
  Printf.printf "canvas right:  %f\n" canvas_right;
  let top_tile = top_tile zoom canvas_top in
  let bottom_tile = bottom_tile zoom canvas_bottom in
  let left_tile = left_tile zoom canvas_left in
  let right_tile = right_tile zoom canvas_right in
  Printf.printf "top left tile:     http://tile.localhost/landscape/%d/%d/%d.png\n" zoom left_tile top_tile;
  Printf.printf "bottom right tile: http://tile.localhost/landscape/%d/%d/%d.png\n" zoom right_tile bottom_tile;
  let osm_base_url = "http://tile.localhost/landscape/{z}/{x}/{y}.png" in
  let osm_url zoom x y =
    let str = osm_base_url in
    let _, str = ExtLib.String.replace ~str ~sub:"{z}" ~by:(string_of_int zoom) in
    let _, str = ExtLib.String.replace ~str ~sub:"{x}" ~by:(string_of_int x   ) in
    let _, str = ExtLib.String.replace ~str ~sub:"{y}" ~by:(string_of_int y   ) in
    str in
  let tmp_dir =
    Random.self_init ();
    let rand = Random.int 1073741823 in
    sprintf "/tmp/gpxer-%d" rand in
  let tiles =
    let rec xloop x acc =
      let rec yloop y acc =
        if y > bottom_tile
        then acc
        else
          let dir = sprintf "%s/%d/%d" tmp_dir zoom x in
          let file = sprintf "%s/%d.png" dir y in
          let url = osm_url zoom x y in
          ignore (Sys.command (sprintf "mkdir -p %s" dir));
          printf "download tile %s\n%!" url;
          ignore (Sys.command (sprintf "curl -s %s -o %s" url file));
          let tile = { file; x; y } in
          yloop (succ y) (tile :: acc) in
      if x > right_tile
      then acc
      else
        let column = yloop top_tile [] in
        xloop (succ x) (List.rev_append column acc) in
    xloop left_tile [] in
  ()

