(* xmlm is the only working OCaml library for parsing XML *)
(* there was awesome http://tech.motion-twin.com/xmllight.html *)
(* but it died 11 years ago *)
(* and can't handle CDATA shit among tags *)
(* and xmlm was writen by fairy-tale junkies *)
(* it uses API allowing to mixing up one tag's end with another *)
(* and this is for parsing xml with </closed_tags> everywhere! *)
(* the only function of </closed_tags> is to NOT ALLOW mixing up *)
(* but they made this possible! *)
(* holy crap *)
(* so I have to write this wrapper for xmlm *)

(* actually this wrapper is half-baked shit *)
(* so be careful when using it *)

type 'a parse_result = Parsed of 'a | Failed
exception Bad_tree

let (|||) p1 p2 (s:Xmlm.signal) =
  match p1 s with
  | Failed -> p2 s
  | anything_else -> anything_else

let input_list input p =
  let rec loop acc =
    let signal = Xmlm.input input in
    match signal with
    | `El_end -> Parsed (List.rev acc)
    | signal -> (
      match p input signal with
      | Parsed r -> loop (r::acc)
      | Failed -> raise Bad_tree
    ) in
  loop []

let input_list_ignore_data input p =
  let rec loop acc =
    let signal = Xmlm.input input in
    match signal with
    | `El_end -> Parsed (List.rev acc)
    | `Data s -> loop acc
    | signal -> (
        match p input signal with
        | Parsed r -> loop (r::acc)
        | Failed -> raise Bad_tree
    ) in
  loop []

let close_tag input =
  let rec loop lvl =
    if lvl < 1
    then ()
    else
      let r = Xmlm.input input in
      match r with
      | `El_end -> loop (lvl - 1)
      | `El_start _ -> loop (lvl + 1)
      | _ -> loop lvl in
  loop 1

let input_list_ignore_other input p =
  let rec loop acc =
    let signal = Xmlm.input input in
    match signal with
    | `El_end -> Parsed (List.rev acc)
    | `Data s -> loop acc
    | signal -> (
        match p input signal with
        | Parsed r -> loop (r::acc)
        | Failed -> (
            match signal with
            | `El_start _ -> (close_tag input; loop acc)
            | _ -> loop acc
        )
    ) in
  loop []

let expect_tag name p input (s:Xmlm.signal) =
  match s with
  | `El_start (((_, n), _) as tag) when n = name ->
      p tag input
  | `El_end -> assert false
  | _ -> Failed

let find_tag name input p =
  let rec loop () =
    let signal = Xmlm.input input in
    match signal with
    | `El_start (((_, n), _) as tag) when n = name ->
        p tag input
    | `El_end -> assert false
    | _ -> loop () in
  loop ()

let expect_data f = function
  | `Data s -> Parsed (f s)
  | _ -> Failed
