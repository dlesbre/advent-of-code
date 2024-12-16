(* ==== Puzzle 06 : https://adventofcode.com/2024/day/0  | None -> Some positions6 ====  *)

type tile = Empty | Full | Guard of Grid.DirectionSet.t

let startpos = ref None

let preprocess input =
  let grid = Grid.parse (fun pos char ->
    match char with
    | '.' -> Empty
    | '#' -> Full
    | '^' -> startpos := Some pos; Guard Grid.(DirectionSet.singleton N)
    | _ -> failwith "Invalid input") input in
  grid, Grid.copy grid

open Vec2

let rec patrol grid direction positions pos =
  let newpos = pos +| Grid.vec2_of_direction direction in
  match Grid.get grid newpos with
  | Full -> patrol grid (Grid.rotate_clockwise direction) positions pos
  | Empty ->
      Grid.set grid newpos @@ Guard (Grid.DirectionSet.singleton direction);
      patrol grid direction (positions+1) newpos
  | (Guard s) ->
      if Grid.DirectionSet.mem direction s
      then None (* Loop *)
      else (
        Grid.set grid newpos @@ Guard (Grid.DirectionSet.add direction s);
        patrol grid direction positions newpos
      )
  | exception Grid.Out_of_bounds -> Some positions

let part1 (grid,_) = Option.get @@ patrol grid Grid.N 1 (Option.get !startpos)
let part2 (grid, grid_copy) =
  let startpos = Option.get !startpos in
  Grid.foldi(fun pos elt total ->
  match elt with
  | Guard _ ->
    if pos = startpos then total else
      let grid' = Grid.copy grid_copy in
      Grid.set grid' pos Full;
      if patrol grid' N 1 startpos = None
      then total + 1
      else total
  | _ -> total (* only place obstruction on guard's path *)) 0 grid

let () = register_int ~year:2024 ~day:06 ~preprocess ~part1 ~part2
