(* ==== Puzzle 24 : https://adventofcode.com/2022/day/24 ==== *)


type direction = Up | Down | Left | Right

type space = Empty | Blizzard of direction list | Wall

module IMap = Map.Make(Int)

module Vec2 = struct
  type t = int * int

  let parse_direction = function
    | '^' -> Up
    | 'v' -> Down
    | '<' -> Left
    | '>' -> Right
    | c -> failwith "Wrong direction char"

  let dir2vec2 = function
    | Up ->    ( 0, -1)
    | Down ->  ( 0,  1)
    | Left ->  (-1,  0)
    | Right -> ( 1,  0)

  let dir_opt2vec2 = function
    | None -> (0,0)
    | Some x -> dir2vec2 x

  let add (x1,y1) (x2,y2) = (x1+x2, y1+y2)

  module Map : sig
    val get: int -> space array array
    val pos_end : t
    val can_move : space array array -> t -> bool
    val print : t -> space array array -> unit
  end = struct
    let calculated = ref IMap.empty

    (** Returns a list of all lines in the input, first line as hd *)
    let rec read_all_lines acc =
    try
      let line = read_line () in
      read_all_lines (line::acc)
    with End_of_file ->
      List.rev acc

    let init l =
      let l' = List.map (fun str ->
          Array.map (function
            | '#' -> Wall
            | '.' -> Empty
            | c -> Blizzard([parse_direction c])
          ) (Array.of_seq (String.to_seq str))
        ) l in
      Array.of_list l'


    let len_y, len_x =
      let lines = read_all_lines [] in
      let array = init lines in
      calculated := IMap.add 0 array !calculated;
      Array.length array, Array.length array.(0)

    let wx = len_x - 1
    let wy = len_y - 1
    let wwx = len_x - 2
    let wwy = len_y - 2

    let pos_end = wwx, wy

    let rec update_copy x y init copy =
      match init with
      | [] -> ()
      | t::q ->
          let (x', y') = add (dir2vec2 t) (x,y) in
          let x' =
            if x' = 0
            then wwx
            else if x' = wx then 1 else x'
          in
          let y' =
            if y' = 0
            then wwy
            else if y' = wy then 1 else y'
          in
          (match copy.(y').(x') with
          | Empty -> copy.(y').(x') <- Blizzard([t])
          | Blizzard q' -> copy.(y').(x') <- Blizzard(t::q')
          | Wall -> failwith "Shouldn't happen");
          update_copy x y q copy

    let step array =
      let copy = Array.init len_y (fun y ->
          Array.init len_x (
            fun x -> if array.(y).(x) = Wall then Wall else Empty
          )) in
      for y = 0 to len_y - 1 do
        for x = 0 to len_x - 1 do
          match array.(y).(x) with
          | Blizzard b -> update_copy x y b copy
          | _ -> ()
        done
      done;
      copy

    let rec do_steps n i array =
      if i = n+1 then array
      else
        let array = step array in
        calculated := IMap.add i array !calculated;
        do_steps n (i+1) array

    let get i =
      match IMap.find_opt i !calculated with
      | Some x -> x
      | None ->
          let (i', array) = IMap.max_binding !calculated in
          do_steps i (i'+1) array

    let rec can_move array (x,y) =
      0 <= x && x < len_x && 0 <= y && y < len_y && array.(y).(x) = Empty

    let print (x',y') array =
      for y = 0 to wy do
        for x = 0 to wx do
          Format.printf "%s"
          (if x = x' && y = y' then "E" else
            match array.(y).(x) with
            | Wall -> "#"
            | Empty -> "."
            | Blizzard [Up] -> "^"
            | Blizzard [Left] -> "<"
            | Blizzard [Right] -> ">"
            | Blizzard [Down] -> "v"
            | Blizzard t -> string_of_int (List.length t)
          )
        done;
        Format.printf "\n"
      done
  end
end

let pos_0 = (1,0)

module ISet = Set.Make(struct
  type t = Vec2.t
  let compare = compare
end)

let options = List.map Vec2.dir_opt2vec2 [Some Left; Some Right; Some Up; Some Down; None]

let imap_add i pos map =
  match IMap.find_opt i map with
  | None -> IMap.add i (ISet.singleton pos) map
  | Some s -> IMap.add i (ISet.add pos s) map

let imap_rm i pos map =
  let s = IMap.find i map in
  let s = ISet.remove pos s in
  if ISet.is_empty s
  then IMap.remove i map
  else IMap.add i s map

let rec bfs end_pos todo next =
  let i, pos_set = IMap.min_binding todo in
  let pos = ISet.min_elt pos_set in
  if pos = end_pos
  then match next with
    | [] -> i
    | t::q -> bfs t (IMap.singleton i (ISet.singleton pos)) q
  else
    let todo = imap_rm i pos todo in
    let i = i+1 in
    let array = Vec2.Map.get i in
    let possible = List.filter (Vec2.Map.can_move array) (List.map (Vec2.add pos) options) in
    bfs end_pos (List.fold_left (fun s p -> imap_add i p s) todo possible) next

let part1 () =
  let s = bfs Vec2.Map.pos_end (IMap.singleton 0 (ISet.singleton (1,0))) [] in
  Format.printf "%d@." s

let part2 () =
  let s = bfs Vec2.Map.pos_end (IMap.singleton 0 (ISet.singleton (1,0))) [(1,0); Vec2.Map.pos_end] in
  Format.printf "%d@." s

let () = part2 ()
