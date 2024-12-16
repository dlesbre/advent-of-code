(* ==== Puzzle 15 : https://adventofcode.com/2024/day/15 ====  *)


type item = Wall | Box of bool (* true for left box *) | Robot | Empty

let robot_init = ref None

let parse_item pos = function
  | '#' -> Wall
  | 'O' -> Box true (* dummy value *)
  | '@' -> assert (!robot_init = None); robot_init:=Some pos; Robot
  | '.' -> Empty
  | _ -> failwith "Wrong format"

let parse_direction = function
  | '^' -> Grid.N
  | '>' -> Grid.E
  | 'v' -> Grid.S
  | '<' -> Grid.W
  | _ -> failwith "Wrong format"

let rec read_all_lines acc =
  try read_all_lines (read_line ()::acc)
  with End_of_file -> List.rev acc

let rec list_split_on f = function
  | [] -> failwith "wrong format"
  | x::xs when f x -> [], xs
  | x::xs -> let a,b= list_split_on f xs in x::a, b

let preprocess lines =
  let grid, dirs = list_split_on (fun x -> x = "") lines in
  let grid = Grid.parse parse_item grid in
  let dirs = List.map (fun s -> List.of_seq (String.to_seq s)) dirs
    |> List.flatten
    |> List.map parse_direction in
  let (x,y) = Grid.size grid in
  let duplicate_grid = Grid.init (2*x,y) (fun (x,y) ->
    match Grid.get grid (x/2, y) with
    | Empty -> Empty
    | Wall -> Wall
    | Robot -> if x mod 2 = 0 then Robot else Empty
    | Box _ -> Box (x mod 2 = 0)) in
  grid, duplicate_grid, dirs

(** Move box for part 1:
    Move a box the robot is pushing, or a stack of boxes
    Does not move the robot, returns true if the box was moved
    Here pos_init is position of the first box of the row, which we will teleport
    to the first empty space found. *)
let rec move_box_1 grid pos_init pos_target offset b =
  match Grid.get grid pos_target with
  | Empty ->
      Grid.set grid pos_target (Box true);
      Grid.set grid pos_init Empty;
      true
  | Wall -> false
  | Box _ -> move_box_1 grid pos_init Vec2.(pos_target +| offset) offset b
  | Robot -> failwith "Moving box into robot"

(** For part 2, moving horizontally is similar to P1, but because L/R is distinguished,
    we can't just teleport the first box to the last position, we need to update the whole row *)
let rec move_box_2_horizontal grid pos_init pos_target offset =
  match Grid.get grid pos_target with
  | Empty ->
      Grid.set grid pos_target (Grid.get grid pos_init);
      true
  | Wall -> false
  | Box _ ->
      let has_moved = move_box_2_horizontal grid pos_target Vec2.(pos_target +| offset) offset in
      if has_moved then Grid.set grid pos_target (Grid.get grid pos_init);
      has_moved
  | Robot -> failwith "Moving box into robot"

(** Find positions of both halves of a box from a single half *)
let box_pos pos b =
  let open Vec2 in
  if b
  then (pos, pos +| Grid.vec2_of_direction Grid.E)
  else Vec2.(pos +| Grid.vec2_of_direction Grid.W, pos)

(** Check if we can move box vertically *)
let rec can_move_box_2_vertical grid pos_target offset =
  let open Vec2 in
  match Grid.get grid pos_target with
  | Empty -> true
  | Wall -> false
  | Box b ->
      let box_pos_l, box_pos_r = box_pos pos_target b in
      can_move_box_2_vertical grid (box_pos_l +| offset) offset &&
      can_move_box_2_vertical grid (box_pos_r +| offset) offset
  | Robot -> failwith "Moving box into robot"

(** This recursively moves all boxes. Assumes such a move is possible *)
let rec perform_move_box_2_vertical grid pos_init pos_target offset =
  let open Vec2 in
  begin match Grid.get grid pos_target with
  | Box b ->
      let box_pos_l, box_pos_r = box_pos pos_target b in
      perform_move_box_2_vertical grid box_pos_l (box_pos_l +| offset) offset;
      perform_move_box_2_vertical grid box_pos_r (box_pos_r +| offset) offset;
  | _ -> ()
  end;
  Grid.set grid pos_target (Grid.get grid pos_init);
  Grid.set grid pos_init Empty

let move_box_2 grid pos_init pos_target ((_,y) as offset) b =
  let open Vec2 in
  if y = 0 then
    move_box_2_horizontal grid pos_init pos_target offset
  else begin
    let box_pos_l, box_pos_r = box_pos pos_init b in
    let can_move =
      can_move_box_2_vertical grid (box_pos_l +| offset) offset &&
      can_move_box_2_vertical grid (box_pos_r +| offset) offset in
    if can_move then (
      perform_move_box_2_vertical grid box_pos_l (box_pos_l +| offset) offset;
      perform_move_box_2_vertical grid box_pos_r (box_pos_r +| offset) offset;
    );
    can_move
  end

let move_robot move_box grid pos direction =
  let offset = Grid.vec2_of_direction direction in
  let npos = Vec2.(pos +| offset) in
  match Grid.get grid npos with
  | Empty ->
      Grid.set grid pos Empty;
      Grid.set grid npos Robot;
      npos
  | Wall -> pos
  | Box b ->
      if move_box grid npos Vec2.(npos +| offset) offset b then (
          Grid.set grid pos Empty;
          Grid.set grid npos Robot;
          npos)
      else pos
  | Robot -> failwith "Duplicate robot"


let grid_score grid =
  Grid.foldi (fun (x,y) elt total ->
    match elt with
    | Box true -> 100*y + x + total
    | _ -> total) 0 grid


let part1 (grid, _, dirs) =
  let _final_pos = List.fold_left (move_robot move_box_1 grid) (Option.get !robot_init) dirs in
  (grid_score grid)

let part2 (_, duplicate_grid, dirs) =
  let (x, y) = (Option.get !robot_init) in
  let _final_pos = List.fold_left (move_robot move_box_2 duplicate_grid) (2*x,y) dirs in
  grid_score duplicate_grid

let () = register_int ~year:2024 ~day:15 ~preprocess ~part1 ~part2
