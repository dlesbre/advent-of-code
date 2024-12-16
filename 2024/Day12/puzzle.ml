(* ==== Puzzle 12 : https://adventofcode.com/2024/day/12 ====  *)

let explore_component_1 grid seen char pos =
  let rec depth_explore pos elt (perimeter, area) =
    match elt with
    | None -> (perimeter+1, area)
    | Some n when n = char ->
              if Hashtbl.mem seen pos then (perimeter, area)
              else (
                Hashtbl.add seen pos ();
                Grid.fold_adjacent_opt (fun _ -> depth_explore) (perimeter, area+1) pos grid)
    | Some _ -> (perimeter+1, area)
  in depth_explore pos (Some char) (0,0)

let explore_with explore grid =
  let seen = Hashtbl.create (let n = Grid.lines grid in n*n) in
  Grid.foldi (fun pos elt sum ->
    if Hashtbl.mem seen pos then sum
    else
      let (perimeter, area) = explore grid seen elt pos in
      sum + perimeter * area
    ) 0 grid

(** Given a list of segment (number i represent segment i..i+1) on a single line
    [nb_sides list] is the number of connected components (merging adjacent segments) *)
let nb_sides list =
  let rec aux previous total = function
    | [] -> total
    | x::rs when x = previous + 1 -> aux x total rs
    | x::rs -> aux x (total + 1) rs
  in aux (-5) 0 (List.sort Int.compare list)
  (* -5 is guaranteed to be smaller than all elements *)

let id_of_direction dir (x,y) =
  let open Grid in
  match dir with
  | N | S -> (dir, y), x
  | E | W -> (dir, x), y

let hashtbl_add_or_incr table pos x = match Hashtbl.find table pos with
  | n -> Hashtbl.replace table pos (x::n)
  | exception Not_found -> Hashtbl.add table pos [x]

let pp fmt l = Format.fprintf fmt "[%a]" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ") Format.pp_print_int) l

let explore_component_2 grid seen char pos =
  let sides = Hashtbl.create 50 in (* map (int, direction) -> List of sides there *)
  let rec depth_explore dir pos elt area =
    match elt with
    | Some n when n = char ->
              if Hashtbl.mem seen pos then area
              else (
                Hashtbl.add seen pos ();
                Grid.fold_adjacent_opt depth_explore (area+1) pos grid)
    | Some _ | None ->
          let id, p = id_of_direction dir pos in
          hashtbl_add_or_incr sides id p; area
  in
  let area = depth_explore (Grid.N) pos (Some char) 0 in
  Hashtbl.fold (fun _ l total -> total + nb_sides l) sides 0, area

let preprocess input = Grid.parse (fun _ -> Fun.id) input
let part1 grid = explore_with explore_component_1 grid
let part2 grid = explore_with explore_component_2 grid

let () = register_int ~year:2024 ~day:12 ~preprocess ~part1 ~part2
