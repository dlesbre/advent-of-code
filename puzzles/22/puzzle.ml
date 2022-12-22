(* ==== Puzzle 22 : https://adventofcode.com/2022/day/22 ==== *)

type tile = Empty | Free | Wall

type rotate = L | R

module Direction : sig
  type t = Up | Down | Left | Right

  val rotate : rotate -> t -> t
  val score : t -> int

end = struct
  type t = Up | Down | Left | Right

  let score = function
  | Right -> 0
  | Down -> 1
  | Left -> 2
  | Up -> 3

  let rotate_right = function
  | Right -> Down
  | Down -> Left
  | Left -> Up
  | Up -> Right

  let rotate_left = function
  | Right -> Up
  | Up -> Left
  | Left -> Down
  | Down -> Right

  let rotate = function
  | R -> rotate_right
  | L -> rotate_left
end

module Vec2 : sig
  type t
  val of_direction : Direction.t -> t
  val add : t -> t -> t
  val neg : t -> t

  module Map : sig
    type pos
    type map

    val init_map: tile array array -> map * pos
    val score: pos -> int
    val rotate : pos -> rotate -> pos
    val move: pos -> map -> int -> pos

    val print : pos -> unit
  end
end = struct
  type t = int * int

  let of_direction =
    let open Direction in
    function
    | Up ->    ( 0, -1)
    | Down ->  ( 0,  1)
    | Left ->  (-1,  0)
    | Right -> ( 1,  0)

  let x = fst
  let y = snd

  let add (x1,y1) (x2,y2) = (x1+x2, y1+y2)
  let neg (x,y) = (-x,-y)

  module Map = struct
    type pos = t * Direction.t
    type map = tile array array

    exception Found of int

    let init_map map =
      try
        Array.iteri (fun i k -> match k with
          | Free -> raise (Found i)
          | _ -> ()
        ) map.(0);
        failwith "Not found"
      with Found i -> map, ((i, 0), Direction.Right)

    let face (x,y) =
      if y < 50 && x < 100 then 1 else
      if y < 50 then 6 else
      if y < 100 then 2 else
      if y < 150 && x >= 50 then 3 else
      if y < 150 then 4 else
      if y < 200 then 5 else
      failwith "not a face"

    let score ((x,y),d) = 4*(x+1) + 1000*(y+1) + Direction.score d

    let rotate (p,d) r = (p, Direction.rotate r d)

    let get (x,y) map = map.(y).(x)

    let in_bounds (x,y) map =
      0 <= y && y < Array.length map &&
      0 <= x && x < Array.length map.(y)
      && map.(y).(x) <> Empty

    let rec wrap_around (x,y) dir map =
      let open Direction in
      Format.printf "Transition@.";
      match face (x,y), dir with
      | 1, Up -> ((0, 100+x), Right)
      | 1, Left -> ((0, 149-y), Right)
      (* | 1, Right -> ((0, 200+y), Right) *)
      | 2, Left -> ((y-50,100), Down)
      | 2, Right -> ((y+50, 49), Up)
      | 3, Right -> ((149, 149-y), Left)
      | 3, Down -> ((49, 100+x), Left)
      | 4, Up -> ((50, 50+x), Right)
      | 4, Left -> ((50, 149-y), Right)
      | 5, Left -> ((y-100, 0), Down)
      | 5, Right -> ((y-100, 149), Up)
      | 5, Down -> ((x+100, 0), Down)
      (* | 6, Left -> ((99, y-200), Left) *)
      | 6, Down -> ((99, x-50), Left)
      | 6, Right -> ((99, 149-y), Left)
      | 6, Up -> ((x-100,199), Up)
      | _ -> failwith "Should not happen"

      (* let pos' = add pos dir in
      if in_bounds pos' map
      then wrap_around pos' dir map
      else pos *)

    let find_next (pos, dir) map =
      let d = of_direction dir in
      let pos' = add pos d in
      let pos' =
        if in_bounds pos' map
        then (pos', dir)
        else wrap_around pos dir map in
      if get (fst pos') map = Free then pos' else (pos,dir)

    let rec move pos map x =
      if x = 0
      then pos
      else move (find_next pos map) map (x-1)

    let print ((x,y),d) =
      let (x', y') = of_direction d in
      Format.printf "%d,%d facing %d,%d face %d@." x y x' y' (face (x,y))
  end
end

let parse_map_line map_line =
  let map = Array.of_seq (String.to_seq map_line) in
  Array.map (function
    | ' ' -> Empty
    | '.' -> Free
    | '#' -> Wall
    | _ -> failwith "Unknown map symbol"
  ) map


type move =
  | Number of int
  | Rotate of rotate

(** Returns the suffix of str starting at start *)
let string_suffix str start =
  String.sub str start (String.length str - start)

let rec parse_last_line line =
  if line = "" then [] else
  let m, s =
    if line.[0] = 'L' then Rotate L,1 else
    if line.[0] = 'R' then Rotate R,1 else
    let i = Scanf.sscanf line "%d" (fun x -> x) in
    Number i, String.length (string_of_int i)
  in m::(parse_last_line (string_suffix line s))

let rec read_all_lines acc =
  let line = read_line () in
  if line = "" then begin
    let array = Array.of_list (List.rev_map parse_map_line acc) in
    array, parse_last_line (read_line ())
  end else
    read_all_lines (line::acc)


let part1 () =
  let map, path = read_all_lines [] in
  let map, pos = Vec2.Map.init_map map in
  let pos = List.fold_left (fun pos mv ->
    Vec2.Map.print pos;
    match mv with
    | Number n -> Format.printf "Forward %d@." n; Vec2.Map.move pos map n
    | Rotate r -> Vec2.Map.rotate pos r
  ) pos path
  in
  Format.printf "%d@." (Vec2.Map.score pos)

let () = part1 ()
