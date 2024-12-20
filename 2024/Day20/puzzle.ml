(* ==== Puzzle 20 : https://adventofcode.com/2024/day/20 ====  *)

type tile = Empty | Wall

let startpos = ref None
let endpos = ref None

(** Compute a grid where [g[x,y]] is length of path to the end *)
let rec dist_to_end grid scores score pos  =
  match Grid.get grid pos with
  | Empty -> if Grid.get scores pos < score then scores
             else (
               Grid.set scores pos score;
               Grid.fold_adjacent (fun _ pos _ scores ->
                dist_to_end grid scores (score+1) pos) scores pos grid
             )
  | Wall | exception Grid.Out_of_bounds -> scores
let dist_to_end grid =
  let scores = Grid.make (Grid.size grid) Int.max_int in
  dist_to_end grid scores 0 (Option.get !endpos)

let preprocess input =
  let grid = Grid.parse (fun pos c ->
      match c with
      | '#' -> Wall
      | '.' -> Empty
      | 'S' -> startpos := Some pos; Empty
      | 'E' -> endpos := Some pos; Empty
      | _ -> failwith "invalid format") input
  in dist_to_end grid


let part1 dist =
  Grid.foldi (fun pos elt total ->
    if elt = Int.max_int then total else
    Grid.direction_fold (fun dir total ->
      let npos = Vec2.(pos +| 2 *| Grid.vec2_of_direction dir) in
      match Grid.get_opt dist npos with
      | Some n when n < elt - 2 ->
        if elt-n-2 >= if !test then 20 else 100 then total+1 else total
      | _ -> total
      ) total
    ) 0 dist

let search = Range.interval (-20) (21)

let part2 dist = Grid.sum (fun pos elt ->
  if elt = Int.max_int then 0 else
  Range.sum (fun dx ->
    let remain = 20 - abs dx in
    Range.count (fun dy  ->
      let offset = (dx,dy) in
      let distance = Vec2.norm_manhattan offset in
      if distance > 20 then assert false;
      match Grid.get_opt dist Vec2.(pos +| offset) with
      | Some n when n < elt - distance -> elt-n-distance >= if !test then 50 else 100
      | _ -> false)
      (Range.interval (-remain) (remain+1))
    ) search
  ) dist

let () = register_int ~year:2024 ~day:20 ~preprocess ~part1 ~part2
