(* ==== Puzzle 04 : https://adventofcode.com/2024/day/4 ====  *)

let preprocess lines = Grid.parse (fun _ c -> c) lines


open Vec2

let part1 grid =
  Grid.foldi (fun root elt total ->
    if Char.equal elt 'X' then
    Grid.direction8_fold (fun dir total ->
      let offset = Grid.vec2_of_direction8 dir in
      match Grid.get_opt grid (3 *| offset +| root) with
      | Some 'S' ->
          if Char.equal (Grid.get grid (offset +| root)) 'M'
             && Char.equal (Grid.get grid (2 *| offset +| root)) 'A'
                then total + 1
                 else total
      | _ -> total
      ) total
    else total
    ) 0 grid

let (let*) = Option.bind

let diag1 = Grid.vec2_of_direction8 Grid.NE
let diag1' = Grid.vec2_of_direction8 Grid.SW
let diag2 = Grid.vec2_of_direction8 Grid.SE
let diag2' = Grid.vec2_of_direction8 Grid.NE

let is_m_or_s c = c = 'M' || c = 'S'

let check_diag grid root diag =
  match
    let* c = Grid.get_opt grid (root +| diag) in
    if is_m_or_s c then
      let* c' = Grid.get_opt grid (root +| (-1 *| diag)) in
      if c' != c && is_m_or_s c' then Some () else None
    else None
  with
    | None -> 0
    | Some () -> 1

let part2 grid =
  Grid.foldi (fun root elt total ->
    if Char.equal elt 'A' then
      total + (check_diag grid root diag1) * (check_diag grid root diag2)
    else total
    ) 0 grid

let () = register_int ~year:2024 ~day:04 ~preprocess ~part1 ~part2
