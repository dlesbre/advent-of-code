(* ==== Puzzle 12 : https://adventofcode.com/2025/day/12 ====  *)

type shape = {
  x: int;
  y: int;
  contains: int list;
}

let parse_shape shape = match String.split_on_char ':' shape with
  | [size; reqs] ->
      let x, y = Scanf.sscanf size "%dx%d" (fun x y -> x,y) in
      let reqs = String.trim reqs
              |> String.split_on_char ' '
              |> List.map int_of_string in
      { x; y; contains=reqs }
  | _ -> failwith "wrong size"

let parse_present = function
  | id::rest ->
      let id = string_slice ~stop:(-1) id |> int_of_string in
      id, Grid.parse (fun _ -> (=) '#') rest
  | _ -> failwith "wrong format"

let preprocess input =
  match list_split ((=) "") input |> List.rev with
  | [] -> failwith "Empty input"
  | shapes::presents -> List.map parse_shape shapes, List.rev_map parse_present presents

(** This only works on the input: just check wether the area is large enough
    to contain all shapes. *)
let part1 (shapes, grids) =
  let grid_sizes = List.map (fun (_, g) -> Grid.count (fun _ c -> c) g) grids in
  list_count (fun shape ->
    let area = shape.x * shape.y in
    let total =List.fold_left2 (fun total wanted size -> total + wanted*size) 0 shape.contains grid_sizes in
    total <= area
  ) shapes

let part2 _ = 0

let () = register_int ~year:2025 ~day:12 ~preprocess ~part1 ~part2
