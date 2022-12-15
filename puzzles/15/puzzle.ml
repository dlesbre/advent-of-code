(* ==== Puzzle 15 : https://adventofcode.com/2022/day/15 ==== *)




module Vec2 = struct
  module T = struct
    type t = int * int
    let compare = compare

  end

  include T
  module Set = Set.Make(T)

  type direction = Up | Down | Left | Right

  let parse_direction = function
    | 'U' -> Up
    | 'D' -> Down
    | 'L' -> Left
    | 'R' -> Right
    | c -> failwith "Wrong direction char"

  let dir2vec2 = function
    | Up ->    ( 1,  0)
    | Down ->  (-1,  0)
    | Left ->  ( 0, -1)
    | Right -> ( 0,  1)

  let x = fst
  let y = snd

  let add (x1,y1) (x2,y2) = (x1+x2, y1+y2)
  let sub (x1,y1) (x2,y2) = (x1-x2, y1-y2)
  let mul (x1,y1) (x2,y2) = (x1*x2, y1*y2)


  let abs (x,y) = abs x, abs y

  let norm_infinite (x,y) = let (x,y) = abs (x,y) in max x y
  let norm_manhattan (x,y) = let (x,y) = abs (x,y) in x + y

  let distance norm a b = norm (sub a b)

  let distance_infinite = distance norm_infinite
  let distance_manhattan = distance norm_manhattan

  let scale s (x,y) = (s*x, s*y)
  let scalar (x1,y1) (x2,y2) = x1 * y2 - x2 * y1

  let add_offset p d = add p (dir2vec2 d)

  module Vec2Not = struct
    let ( +@ ) = add
    let ( -@ ) = sub
    let ( *@ ) = scale
    let ( *. ) = scalar
  end
end


type measurment = {
  sensor_pos : Vec2.t;
  beacon_pos : Vec2.t;
  sensor_range : int
}

let parse_line line =
  Scanf.sscanf line "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
  (fun a b c d -> {
    sensor_pos=a,b;
    beacon_pos=c,d;
    sensor_range=Vec2.distance_manhattan (a,b) (c,d);
  })

(** Returns a list of all lines in the input, first line as hd *)
let rec read_all_lines acc =
  try
    let line = read_line () in
    let contents = parse_line line in
    read_all_lines (contents::acc)
  with End_of_file ->
    List.rev acc

let rec find_pos x x' y distance set =
  if distance < 0 then set
  else
    let set = Vec2.Set.add (x, y) set in
    let set = Vec2.Set.add (x',y) set in
    (* Format.printf "Adding (%d,%d) and (%d,%d)@." x y x' y; *)
    find_pos (x+1) (x'-1) y (distance-1) set

(** Adds cleared positions to the set given as argument *)
let no_beacon_in_pos y pos =
  let sensor_x = Vec2.x pos.sensor_pos in
  let distance_to_y = Vec2.distance_manhattan (sensor_x, y) pos.sensor_pos in
  (* Format.printf "sensor %d, %d  beacon %d, %d  distance %d  scannable %d@."
    sensor_x (Vec2.y pos.sensor_pos) (Vec2.x pos.beacon_pos) (Vec2.y pos.beacon_pos)
    distance_to_y (pos.sensor_range - distance_to_y); *)
  find_pos sensor_x sensor_x y (pos.sensor_range - distance_to_y)

let part1 () =
  let measures = read_all_lines [] in
  let y = 2000000 in
  let cleared = List.fold_left (fun set m ->
      no_beacon_in_pos y m set
    ) Vec2.Set.empty measures in
  let beacons = List.fold_left (fun set m ->
      Vec2.Set.add m.beacon_pos set
    ) Vec2.Set.empty measures in
  Format.printf "%d\n" (Vec2.Set.cardinal (Vec2.Set.diff cleared beacons))

let list_max f =
  List.fold_left (fun acc x -> max acc (f x)) min_int

let can_contain_beacon measures x y =
  list_max (fun m ->
    let dist = Vec2.distance_manhattan (x,y) m.sensor_pos in
    (* Format.printf "looking at %d, %d from %d, %d, range %d, dist %d@."
      x y (Vec2.x m.sensor_pos) (Vec2.y m.sensor_pos) m.sensor_range
      dist; *)
    if m.sensor_range >= dist
    then let x_diff = Vec2.x m.sensor_pos - x in
      max (2*x_diff) (m.sensor_range - dist) + 1
    else -1) measures

let max_v = 4_000_000

let rec search measures x y =
  if y > max_v then failwith "Out of bounds" else
  if x > max_v then search measures 0 (y+1) else
  let incr = can_contain_beacon measures x y in
  if incr < 0 then x, y else
    search measures (x+incr) y

let part2 () =
  let measures = read_all_lines [] in
  let x, y = search measures 0 0 in
  Format.printf "(%d, %d) -> %d@." x y (x*4_000_000 + y)

let () = part2 ()
