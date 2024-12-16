(* ==== Puzzle 08 : https://adventofcode.com/2024/day/8 ====  *)

type cell = Empty | Antenna of char

(** Given a set of same-labeled antenna position, return the set of their antinodes *)
let find_antinodes set =
  let open Vec2 in
  set_fold_pairs (module Vec2.Set) (fun a b antinodes ->
    let diff = b -| a in
    (* Format.printf "%a %a : %a and %a@." pp a pp b pp (a -| diff) pp (b +| diff); *)
    antinodes |>
    Vec2.Set.add (a -| diff) |>
    Vec2.Set.add (b +| diff)
  ) set Vec2.Set.empty

let rec add_while_in_bounds grid set x diff =
  if Grid.in_bounds grid x
  then add_while_in_bounds grid (Vec2.Set.add x set) Vec2.(x +| diff) diff
  else set

let find_resonant_antinodes grid set =
  let open Vec2 in
  set_fold_pairs (module Vec2.Set) (fun a b antinodes ->
    let diff = b -| a in
    (* Format.printf "%a %a : %a and %a@." pp a pp b pp (a -| diff) pp (b +| diff); *)
    let antinodes = add_while_in_bounds grid antinodes (a -| diff) (~| diff) in
    add_while_in_bounds grid antinodes (b +| diff) diff
  ) set
  (if Vec2.Set.cardinal set = 1 then Vec2.Set.empty else set)

let preprocess input =
  Grid.parse (fun _ c -> if c='.' then Empty else Antenna c) input,
  Hashtbl.create 50

let part1 (grid, table) =
  Grid.iteri (fun pos elt -> match elt with
  | Empty -> ()
  | Antenna c -> match Hashtbl.find table c with
      | s -> Hashtbl.replace table c (Vec2.Set.add pos s)
      | exception Not_found -> Hashtbl.add table c (Vec2.Set.singleton pos)
  ) grid;
  let antinodes = Hashtbl.fold (fun _ s antinodes ->
    Vec2.Set.union (find_antinodes s) antinodes
    ) table Vec2.Set.empty in
  let antinodes = Vec2.Set.filter (Grid.in_bounds grid) antinodes in
  Vec2.Set.cardinal antinodes

let part2 (grid, table) =
  let resonant_antinodes = Hashtbl.fold (fun _ s antinodes ->
    Vec2.Set.union (find_resonant_antinodes grid s) antinodes
    ) table Vec2.Set.empty in
  let resonant_antinodes = Vec2.Set.filter (Grid.in_bounds grid) resonant_antinodes in
  Vec2.Set.cardinal resonant_antinodes

let () = register_int ~year:2024 ~day:08 ~preprocess ~part1 ~part2
