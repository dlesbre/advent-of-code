(* ==== Puzzle 23 : https://adventofcode.com/2022/day/23 ==== *)

type direction = N | S | W | E

module Vec2Map = Map.Make(struct
  type t = int * int
  let compare = compare
end)

module Vec2Set = struct
  include Set.Make(struct
  type t = int * int
  let compare = compare
end)
  let rec doesnt_contain set = function
    | [] -> true
    | x::q -> not (mem x set) && doesnt_contain set q
end

let rotate direction_list =
  List.tl direction_list @ [ List.hd direction_list ]

let can_move_direction (x,y) elves = function
  | N -> Vec2Set.doesnt_contain elves [(x-1,y-1); (x,y-1); (x+1,y-1)]
  | S -> Vec2Set.doesnt_contain elves [(x-1,y+1); (x,y+1); (x+1,y+1)]
  | W -> Vec2Set.doesnt_contain elves [(x-1,y-1); (x-1,y); (x-1,y+1)]
  | E -> Vec2Set.doesnt_contain elves [(x+1,y-1); (x+1,y); (x+1,y+1)]

let move_dir (x,y) = function
  | N -> (x, y-1)
  | S -> (x, y+1)
  | W -> (x-1, y)
  | E -> (x+1, y)

let move_elf pos elves direction =
  let possible = List.filter (can_move_direction pos elves) direction in
  if List.length possible = 4 || List.length possible = 0
  then None
  else Some (move_dir pos (List.hd possible))

let do_one_round elves direction =
  (* part1 find out where everyone moves, and if they are the only one to move there*)
  let moves, wants = Vec2Set.fold
    (fun pos (moves, wants) ->
      let want = move_elf pos elves direction in
      match want with
      | None -> moves, wants
      | Some pos' ->
          let moves = Vec2Map.add pos pos' moves in
          match Vec2Map.find_opt pos' wants with
          | None -> moves, Vec2Map.add pos' true wants (* no one is moving there (yet) *)
          | Some _ -> moves, Vec2Map.add pos' false wants (* at least another elf wants to go here *)
      ) elves (Vec2Map.empty, Vec2Map.empty) in
  (* part 2, move everyone *)
  let elves, moved = Vec2Set.fold
    (fun pos (new_pos, moved) ->
      match Vec2Map.find_opt pos moves with
      | None -> Vec2Set.add pos new_pos, moved
      | Some pos' ->
        if Vec2Map.find pos' wants
        then Vec2Set.add pos' new_pos, true
        else Vec2Set.add pos new_pos, moved
    ) elves (Vec2Set.empty, false) in
  elves, rotate direction, moved

let free_area elves =
  let min_x, min_y, max_x, max_y =
    Vec2Set.fold (fun (x,y) (min_x, min_y, max_x, max_y) ->
      min x min_x,
      min y min_y,
      max x max_x,
      max y max_y
    ) elves (max_int, max_int, min_int, min_int) in
    (max_x - min_x + 1) * (max_y - min_y + 1) - Vec2Set.cardinal elves

(** Returns a list of all lines in the input, first line as hd *)
let rec read_all_lines acc =
  try
    let line = read_line () in
    read_all_lines (line::acc)
  with End_of_file ->
    List.rev acc

let make_elves_set lines =
  let (_, set) = List.fold_left (fun (y,set) line ->
    let (_, set) = String.fold_left (fun (x,set) c ->
        if c = '#'
        then (x+1, Vec2Set.add (x,y) set)
        else (x+1, set)
      ) (0,set) line
    in (y+1, set)
  ) (0, Vec2Set.empty) lines in set

(* let print elves =
  let min_x, min_y, max_x, max_y =
  Vec2Set.fold (fun (x,y) (min_x, min_y, max_x, max_y) ->
    min x min_x,
    min y min_y,
    max x max_x,
    max y max_y
  ) elves (max_int, max_int, min_int, min_int) in
  for k = min_x to *)

let rec do_n_rounds n elves direction =
  if n = 0 then elves
  else
    let elves, direction, _ = do_one_round elves direction in
    do_n_rounds (n-1) elves direction

let rec do_while_move nb elves direction =
  let elves, direction, moved = do_one_round elves direction in
  if moved
  then do_while_move (nb+1) elves direction
  else nb

let part1 () = (* 0.16s *)
  let lines = read_all_lines [] in
  let elves = make_elves_set lines in
  let elves = do_n_rounds 10 elves [N;S;W;E] in
  Format.printf "%d\n" (free_area elves)

let part2 () = (* ~20s *)
  let lines = read_all_lines [] in
  let elves = make_elves_set lines in
  let nb = do_while_move 1 elves [N;S;W;E] in
  Format.printf "%d\n" nb

let () = part2 ()
