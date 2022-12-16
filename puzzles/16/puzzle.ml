(* ==== Puzzle 16 : https://adventofcode.com/2022/day/16 ==== *)

module StringMap = Map.Make(String)

module BitSet : sig
  (* Requires values >= 0 and <= 63 *)
  type t
  val empty : t
  val get : t -> int -> bool
  val set : t -> int -> t
end = struct
  type t = int
  let empty = 0
  let to_offset x = 1 lsl x
  let get t x = t land (to_offset x) <> 0
  let set t x = t lor (to_offset x)
end

type ('a, 'b) valve = {
  name: 'a;
  flow: int;
  neighbors: 'b list;
}

let tmax = 30

let parse_neighbors x =
  List.map String.trim (String.split_on_char ',' x)

(** Returns the suffix of str starting at start *)
let string_suffix str start =
  String.sub str start (String.length str - start)

let parse_line line =
  Scanf.sscanf (line ^ "!")
  "Valve %s has flow rate=%d; tunnel%s lead%s to valve%s %s@!"
  (fun name flow _ _ _ n -> {name; flow; neighbors=parse_neighbors n })

(** Returns a list of all lines in the input, first line as hd *)
let rec read_all_lines acc =
  try
    let line = read_line () in
    let contents = parse_line line in
    read_all_lines (contents::acc)
  with End_of_file ->
    List.fold_left (fun map valve -> StringMap.add valve.name valve map) StringMap.empty acc

(** Turns valve names into unique numbers
    All valves with no flow have numbers larger then the second returned value
    the third returned value is the number for "AA" *)
let intify valves =
  let nb = StringMap.cardinal valves in
  let folder s v = fun (i, j, m) ->
    if v.flow > 0
    then (i+1, j, StringMap.add s i m)
    else (i, j-1, StringMap.add s j m) (* put un-interesting nodes in the back *)
  in
  let non_empty, _, to_int = StringMap.fold folder valves (0, nb-1, StringMap.empty) in
  (* let () = StringMap.iter (fun s i -> Format.printf "Node %s mapped to %d@." s i) to_int in *)
  let to_int x = StringMap.find x to_int in
  let start = to_int "AA" in
  let to_int v = { v with name = to_int v.name; neighbors = List.map to_int v.neighbors } in
  Array.of_list (
    List.sort
      (fun x y -> compare x.name y.name)
      (StringMap.fold (fun _ v l -> to_int v::l ) valves [])),
  non_empty, start

let distances nodes =
  let n = Array.length nodes in
  (* initialize distances *)
  let distances = Array.init n (fun k ->
    let arr = Array.init n (fun j -> if j = k then Some 0 else None) in
    List.iter (fun x -> arr.(x) <- Some 1) nodes.(k).neighbors;
    arr
  ) in
  (* Floyd-Warshall *)
  for k = 0 to n-1 do
    for i = 0 to n-1 do
      for j = 0 to n-1 do
        let dist_ij = distances.(i).(j) in
        let dist_ik = distances.(i).(k) in
        let dist_kj = distances.(k).(j) in
        let dist_ikj = match dist_ik, dist_kj with
          | Some d, Some d' -> Some (d + d')
          | _, _ -> None
        in
        match dist_ikj, dist_ij with
        | Some x, Some y -> distances.(i).(j) <- Some (min x y)
        | Some x, _ -> distances.(i).(j) <- Some x
        | _ -> ()
      done
    done
  done;
  distances

type dist = {
  node: int;
  dist: int;
}

let make_neighbors k distances non_empty =
  let neighbors = Array.mapi (fun i d ->
      if i >= non_empty || i=k then None else
      match d with
      | None -> None
      | Some d -> Some {node=i; dist=d}
    ) distances
  in List.map Option.get (
    List.filter Option.is_some (Array.to_list neighbors)
  )


let make_graph valves =
  let valves, non_empty, start = intify valves in
  let dist = distances valves in
  Array.map (fun v ->
      { v with neighbors = make_neighbors v.name dist.(v.name) non_empty }
    ) valves, start

type state = {
  value: int;
  time: int;
  opens: BitSet.t
}

let step f n = { f with time = f.time + n }

let is_done f = f.time >= tmax

let list_max ?(max=max) f m =
  List.fold_left (fun acc x -> max acc (f x)) m

let open_valve state pos = {
    state with
    value = state.value + (tmax + 1 - state.time)*pos.flow; (* add all gains from this valve until the end *)
    opens = BitSet.set state.opens pos.name;
  }

let rec search state graph pos =
  if is_done state then state.value
  else
    let state = step state 1 in
    let state = open_valve state pos in
    list_max (iter_list state graph) state.value pos.neighbors
and iter_list state graph x =
  let valve = graph.(x.node) in
    if BitSet.get state.opens valve.name || x.dist+1 >= tmax - state.time
    then 0 (* don't go if already opened or if you can't reach in time *)
    else search (step state x.dist) graph valve

let part1 () =
  let valves = read_all_lines [] in
  let graph, start = make_graph valves in
  (* We start at time 0 instead of one since its simpler to open AA and then move on
     then to repeat the move on logic... *)
  let res = search { time=0; value=0; opens=BitSet.empty } graph graph.(start) in
  Format.printf "%d@." res



let rec search state graph pos start first_run =
    let return state =
      if first_run
      then search { state with time=4 } graph start start false (* restart with second actor *)
      else state.value
    in
    if is_done state then return state
    else
      let state = step state 1 in
      let state = open_valve state pos in
      list_max (iter_list state graph start first_run) (return state) pos.neighbors
and iter_list state graph start first_run x =
  let valve = graph.(x.node) in
    if BitSet.get state.opens valve.name || x.dist+1 >= tmax - state.time
    then 0 (* don't go if already opened or if you can't reach in time *)
    else search (step state x.dist) graph valve start first_run

let part2 () =
  let valves = read_all_lines [] in
  let graph, start = make_graph valves in
  let res = search { time=4; value=0; opens=BitSet.empty } graph graph.(start) graph.(start) true in
  Format.printf "%d@." res

let () = part2 ()
