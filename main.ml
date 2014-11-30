
let (|>) f g = g f (* default in OCaml 4.01 *)
let (@@) f x = f x

let rec exude p = function
  | [] -> raise Not_found
  | x :: l -> (match p x with
      | Some y -> y
      | None -> exude p l)

let () =
  let i = Xmlm.make_input (`Channel stdin) in
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
                    Xml.Parsed (lat, lon)
  in
  match r with
  | Xml.Parsed r -> List.iter (List.iter (List.iter (fun (lat, lon) -> Printf.printf "(%s, %s)\n" lat lon))) r
  | Xml.Failed -> assert false (* bad gpx *)
