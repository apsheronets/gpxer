
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
      fun _ i -> Xml.input_list_ignore_data i @@
        Xml.expect_tag "trk" @@
          fun _ i -> Xml.input_list_ignore_data i @@
            Xml.expect_tag "trkseg" @@
              fun _ i -> Xml.input_list_ignore_data i @@
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

let () =
  let gpx = gpx_of_channel stdin in
  print_gpx gpx;
  ignore (
    calc_zoom
      (min_lat gpx)
      (max_lat gpx)
      (min_lon gpx)
      (max_lon gpx)
      300
      300
  )

