(* ==== Puzzle 07 : https://adventofcode.com/2025/day/7 ====  *)

let preprocess input =
  let start = ref None in
  let grid = Grid.parse (fun pos chr -> match chr with
    | 'S' -> start := Some pos; false
    | '^' -> true
    | _ -> false
    ) input in
  grid, Option.get !start

let rec part1 seen grid pos =
  if Grid.in_bounds grid pos && not (Vec2.Set.mem pos seen) then
    let seen = Vec2.Set.add pos seen in
    if Grid.get grid pos
    then
      let seen, left = part1 seen grid Vec2.(pos +| (-1,0)) in
      let seen, right = part1 seen grid Vec2.(pos +| (1,0)) in
      seen, 1 + left + right
    else part1 seen grid Vec2.(pos +| (0,1))
  else seen, 0

let part1 (grid, start) = part1 Vec2.Set.empty grid start |> snd

let rec part2 grid mem pos =
  match Hashtbl.find_opt mem pos with
  | Some n -> n
  | None ->
    let n =
      if Grid.in_bounds grid pos then
        if Grid.get grid pos
        then
          let left = part2 grid mem Vec2.(pos +| (-1,0)) in
          let right = part2 grid mem Vec2.(pos +| (1,0)) in
          left + right
        else part2 grid mem Vec2.(pos +| (0,1))
      else 1
    in Hashtbl.add mem pos n;
    n

let part2 (grid, start) = part2 grid (Hashtbl.create 100) start

let () = register_int ~year:2025 ~day:07 ~preprocess ~part1 ~part2
