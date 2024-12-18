(* ==== Puzzle 16 : https://adventofcode.com/2024/day/16 ====  *)

open AOC

type tile = Wall | Empty

let startpos = ref None
let endpos = ref None

let parse_tile pos = function
  | '#' -> Wall
  | '.' -> Empty
  | 'S' -> startpos := Some pos; Empty
  | 'E' -> endpos := Some pos; Empty
  | _ -> failwith "Invalid input"


(** Use a map score -> list of positions as a priority queue *)

let add_elt i n path imap =
  IntMap.update i (function
    | None -> Some [(n, path)]
    | Some ns -> Some (list_assoc_update (function None -> path | Some p -> Vec2.Set.union p path) n ns))
  imap

(** Perform breadth first search using a priority queue "score -> position*direction*paths" *)
let rec bfs grid seen queue =
  let (score, (n, path), queue) = imap_pop_minimum queue in
  if Hashtbl.mem seen n then bfs grid seen queue
  else
    let (pos, dir) = n in
    if pos = Option.get !endpos then score, path
    else begin
      Hashtbl.replace seen (pos,dir) ();
      let npos = Vec2.(pos +| Grid.vec2_of_direction dir) in
      begin match Grid.get grid npos with
      | Empty -> add_elt (score+1) (npos,dir) (Vec2.Set.add npos path) queue
      | _ -> queue
      end |>
      add_elt (score+1000) (pos, (Grid.rotate_clockwise dir)) path |>
      add_elt (score+1000) (pos, (Grid.rotate_counterclockwise dir)) path |>
      bfs grid seen
    end

let preprocess input =
  let grid = Grid.parse parse_tile input in
  let startpos = Option.get !startpos in
  let table = Hashtbl.create (let n = Grid.lines grid in n*n) in
  bfs grid table @@ IntMap.singleton 0 [(startpos, Grid.E), Vec2.Set.singleton startpos]

let part1 = fst
let part2 (_, paths) = Vec2.Set.cardinal paths

let () = register_int ~year:2024 ~day:16 ~preprocess ~part1 ~part2
