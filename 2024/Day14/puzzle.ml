(* ==== Puzzle 14 : https://adventofcode.com/2024/day/14 ====  *)

type robot = {
  position: Vec2.t;
  velocity: Vec2.t;
}

let parse_line s = Scanf.sscanf s "p=%d,%d v=%d,%d" (fun px py vx vy -> { position=(px,py); velocity=(vx,vy) })

let preprocess = List.map parse_line

let calc_position steps size robot =
  let open Vec2 in
  (robot.position +| steps *| robot.velocity) %| size

let quadrant_sum size robot_positions =
  let mid_x = fst size / 2 in
  let mid_y = snd size / 2 in
  let (qNW, qNE, qSW, qSE) = List.fold_left (fun (qNW, qNE, qSW, qSE) robot ->
    if fst robot < mid_x then
        if snd robot < mid_y then
          (qNW+1, qNE, qSW, qSE)
        else if snd robot > mid_y then
          (qNW, qNE, qSW+1, qSE)
        else
          (qNW, qNE, qSW, qSE)
    else if fst robot > mid_x then
      if snd robot < mid_y then
        (qNW, qNE+1, qSW, qSE)
      else if snd robot > mid_y then
        (qNW, qNE, qSW, qSE+1)
      else
        (qNW, qNE, qSW, qSE)
    else
      (qNW, qNE, qSW, qSE)
    ) (0,0,0,0) robot_positions in
  qNW * qNE * qSE * qSW

let to_grid robots size =
  let grid = Grid.make size 0 in
  List.iter (fun pos ->
    Grid.set grid pos (Grid.get grid pos + 1)
    ) robots;
  grid

exception Not_Symmetric

let is_symmetric grid (size_x,_) =
  match Grid.iteri (fun (px,py) nb ->
    if Grid.get grid (size_x - px - 1, py) = nb
    then ()
    else raise Not_Symmetric
  ) grid with
  | () -> true
  | exception Not_Symmetric -> false

let rec list_print_6 a b c d e f= match a,b,c,d,e,f with
  | [],[],[],[],[],[] -> ()
  | a::a', b::b', c::c', d::d', e::e', f::f' ->
     Format.printf "%s    |    %s    |    %s    |    %s    |    %s    |    %s@." a b c d e f;
     list_print_6 a' b' c' d' e' f'
  | _ -> failwith "Mismatch lenghts"

let break_lines = String.split_on_char '\n'

(* With minimal font, I can print 6 grids side-by-side on my terminal... *)
let rec find_tree robots size n =
  let grid_1 = to_grid (List.map (calc_position n size) robots) size in
  let grid_2 = to_grid (List.map (calc_position (n+1) size) robots) size in
  let grid_3 = to_grid (List.map (calc_position (n+2) size) robots) size in
  let grid_4 = to_grid (List.map (calc_position (n+3) size) robots) size in
  let grid_5 = to_grid (List.map (calc_position (n+4) size) robots) size in
  let grid_6 = to_grid (List.map (calc_position (n+5) size) robots) size in
  let s1 = Format.asprintf "Grid at step %d:@.%a@." n (Grid.pp (fun fmt n -> if n = 0 then Format.fprintf fmt " " else Format.fprintf fmt "#")) grid_1 in
  let s2 = Format.asprintf "Grid at step %d:@.%a@." (n+1) (Grid.pp (fun fmt n -> if n = 0 then Format.fprintf fmt " " else Format.fprintf fmt "#")) grid_2 in
  let s3 = Format.asprintf "Grid at step %d:@.%a@." (n+2) (Grid.pp (fun fmt n -> if n = 0 then Format.fprintf fmt " " else Format.fprintf fmt "#")) grid_3 in
  let s4 = Format.asprintf "Grid at step %d:@.%a@." (n+3) (Grid.pp (fun fmt n -> if n = 0 then Format.fprintf fmt " " else Format.fprintf fmt "#")) grid_4 in
  let s5 = Format.asprintf "Grid at step %d:@.%a@." (n+4) (Grid.pp (fun fmt n -> if n = 0 then Format.fprintf fmt " " else Format.fprintf fmt "#")) grid_5 in
  let s6 = Format.asprintf "Grid at step %d:@.%a@." (n+5) (Grid.pp (fun fmt n -> if n = 0 then Format.fprintf fmt " " else Format.fprintf fmt "#")) grid_6 in
  list_print_6 (break_lines s1) (break_lines s2) (break_lines s3) (break_lines s4) (break_lines s5) (break_lines s6);
  Unix.sleepf 0.2;
  find_tree robots size (n+6)

let explore_component grid seen elt pos =
  let rec depth_explore pos elt area =
    match elt with
    | 0 -> area
    | _ -> if Hashtbl.mem seen pos then area
           else (
              Hashtbl.add seen pos ();
              Grid.fold_adjacent (fun _ -> depth_explore) (area+1) pos grid)
  in depth_explore pos elt 0


let rec find_tree_smart robots size n =
  let grid = to_grid (List.map (calc_position n size) robots) size in
  let seen = Hashtbl.create (let n = Grid.lines grid in n*n) in
  let max_size = Grid.foldi (fun pos elt max_size ->
    if elt = 0 then max_size
    else max max_size (explore_component grid seen elt pos)) 0 grid in
  if max_size > 100 then (
    Format.printf "Grid at step %d:@.%a@." n (Grid.pp (fun fmt n -> if n = 0 then Format.fprintf fmt " " else Format.fprintf fmt "#")) grid;
    n)
  else find_tree_smart robots size (n+1)

let test_size = (11,7)
let true_size = (101,103)
let size = true_size
let part1 robots =
  let new_positions = List.map (calc_position 100 size) robots in
  quadrant_sum size new_positions

let part2 robots = find_tree_smart robots size 0

let () = register_int ~year:2024 ~day:14 ~preprocess ~part1 ~part2
