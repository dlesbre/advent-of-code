(* ==== Puzzle 12 : https://adventofcode.com/2022/day/12 ==== *)

type cell = {
  height: int;
  is_start: bool;
  is_end: bool;
}

let get_height = function
  | 'S' -> 0
  | 'E' -> 25
  | x -> int_of_char x - int_of_char 'a'

(** Returns a list of all lines in the input, first line as hd *)
let rec read_all_lines acc =
  try
    let line = read_line () in
    read_all_lines (line::acc)
  with End_of_file ->
    Array.of_list (List.map (fun line ->
        Array.init (String.length line) (fun i ->
          {
            height = get_height line.[i];
            is_start = line.[i] = 'S';
            is_end = line.[i] = 'E'
          }

          ))
      (List.rev acc))



let height_map = read_all_lines []

exception Found of int * int

let rec find f arr =
  try
    Array.iteri (
      fun i a -> Array.iteri (
        fun j c -> if f c then raise (Found (i,j))
      ) a
    ) arr;
    failwith "not found"
  with Found(i,j) -> i, j

let start = find (fun x -> x.is_start) height_map
let end_ = find (fun x -> x.is_end) height_map

let height = Array.length height_map
let width = Array.length height_map.(0)


(* For part 2 we proceed in reverse: from the end to the start
   so we can only decrease height by at most 1 *)
let can_move (i,j) (k,l) = height_map.(i).(j).height <= height_map.(k).(l).height + 1

let neighbors (i,j) =
  let l = if i + 1 < height && can_move (i,j) (i+1,j) then [i+1,j] else [] in
  let l = if i > 0 && can_move (i,j) (i-1,j) then (i-1,j)::l else l in
  let l = if j + 1 < width && can_move (i,j) (i,j+1) then (i,j+1)::l else l in
  if j > 0 && can_move (i,j) (i,j-1) then (i,j-1)::l else l

let to_unique (i,j) =
  i*width + j

type vertex = {
  distance: int option;
  pos: int*int
}
module VertexSet = Set.Make(struct type t = vertex let compare = compare end)

let opt_incr = function
  | None -> None
  | Some x -> Some (x+1)

let ( >== ) a b = match a, b with (* None is extra max element *)
  | None, _ -> true
  | _, None -> false
  | Some x, Some y -> x >= y

let dijkstra start =
  let n = height * width in
  let min_distance = Array.make n None in
  min_distance.(to_unique start) <- Some 0;
  let make_vertex v = { distance = min_distance.(to_unique v); pos = v } in
  (* let previous = Array.make n (-1) in *)
  let rec aux vertex_queue =
    if not (VertexSet.is_empty vertex_queue) then
      let u = VertexSet.min_elt vertex_queue in
      let vertex_queue' = VertexSet.remove u vertex_queue in
      let edges = neighbors u.pos in
      let f vertex_queue v =
        let v_i = to_unique v in
        let dist_thru_u = opt_incr u.distance in
        if dist_thru_u >== min_distance.(v_i) then vertex_queue
        else begin
          let vertex_queue' = VertexSet.remove (make_vertex v) vertex_queue in
          min_distance.(to_unique v) <- dist_thru_u;
          (* previous.(v) <- u; *)
          VertexSet.add (make_vertex v) vertex_queue'
        end
      in
      aux (List.fold_left f vertex_queue' edges)
    in
    aux (VertexSet.singleton (make_vertex start));
    min_distance

  (* let shortest_path_to (target : vertex) (previous : vertex array) : vertex list =
    let rec aux target acc =
      if target = -1 then
        acc
      else
        aux previous.(target) (target :: acc)
    in
    aux target [] *)

let part1 () =
  let distance = dijkstra end_ in
  Format.printf "%d\n" (Option.get distance.(to_unique start))

let part2 () =
  let distance = dijkstra end_ in
  let mini = ref max_int in
  Array.iteri (fun i line ->
    Array.iteri (fun j c ->
        if c.height = 0 then
          match distance.(to_unique (i,j)) with
          | Some d -> if d <= !mini then mini := d
          | None -> ()
      ) line
    ) height_map;
  Format.printf "%d\n" !mini

let () = part2 ()
