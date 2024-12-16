(* ==== Puzzle 10 : https://adventofcode.com/2024/day/10 ====  *)

(** Score for part 1 *)
let trail_score_1 grid pos =
  let rec follow_trail pos n (total, seen) =
    Grid.direction_fold (fun  dir((total,seen) as acc) ->
      let npos = Vec2.(pos +| Grid.vec2_of_direction dir) in
      if Vec2.Set.mem npos seen then acc
      else
        match Grid.get grid npos with
        | x when x = n ->
            let seen = Vec2.Set.add npos seen in
            if n = 9
            then (total+1, seen)
            else follow_trail npos (n+1) (total, seen)
        | _ -> acc
        | exception Grid.Out_of_bounds -> acc
      ) (total, seen)
  in follow_trail pos 1 (0, Vec2.Set.singleton pos) |> fst

module Vec2Map = Map.Make(Vec2)

let map_incr pos n map = Vec2Map.update pos (function None -> Some n | Some m -> Some (m+n)) map
let find pos map = match Vec2Map.find pos map with
  | n -> n
  | exception Not_found -> 0

(** Score for part 2 *)
let trail_score_2 grid pos =
  let rec follow_trail pos n paths =
    Grid.direction_fold (fun dir paths ->
      let npos = Vec2.(pos +| Grid.vec2_of_direction dir) in
      match Grid.get grid npos with
      | x when x = n ->
          if Vec2Map.mem npos paths
          then map_incr pos (find npos paths) paths
          else if n = 9
            then map_incr pos 1 paths
            else
              let paths = follow_trail npos (n+1) paths in
              map_incr pos (find npos paths) paths
      | _ -> paths
      | exception Grid.Out_of_bounds -> paths
      ) paths
  in follow_trail pos 1 Vec2Map.empty
  |> find pos

let preprocess lines = Grid.parse (fun _ c -> int_of_char c - int_of_char '0') lines
let part1 grid = Grid.foldi (fun pos elt acc ->
    if elt = 0
    then acc + trail_score_1 grid pos
    else acc) 0 grid
let part2 grid = Grid.foldi (fun pos elt acc ->
    if elt = 0
    then acc + trail_score_2 grid pos
    else acc) 0 grid

let () = register_int ~year:2024 ~day:10 ~preprocess ~part1 ~part2
