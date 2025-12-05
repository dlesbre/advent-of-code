(* ==== Puzzle 04 : https://adventofcode.com/2025/day/4 ====  *)

let preprocess = Grid.parse (fun _ c -> c = '@')

let is_accessible pos grid =
    Grid.get grid pos (* contains a roll *) &&
    (Grid.fold_adjacent8 (fun _ _ has_roll sum -> sum + Bool.to_int has_roll) 0 pos grid < 4)

let part1 grid = Grid.count (fun pos _ -> is_accessible pos grid) grid
let rec part2 acc grid =
  let nb_accessible = Grid.foldi
    (fun pos _ acc ->
      if is_accessible pos grid
      then
        let () = Grid.set grid pos false in
        acc+1
      else acc)
    0 grid in
  if nb_accessible = 0 then acc else part2 (acc + nb_accessible) grid
let part2 = part2 0

let () = register_int ~year:2025 ~day:04 ~preprocess ~part1 ~part2
