(* ==== Puzzle 09 : https://adventofcode.com/2025/day/09 ====  *)

let preprocess = List.map Vec2.of_string

let area a b = let (x,y) = Vec2.(a -| b) in (abs x+1) * (abs y+1)

let part1 = list_fold_pairs ~reflexive:false
  (fun max_area p1 p2 -> max max_area (area p1 p2))
  min_int

(** for each [x] in [set], add [x+1] and [x-1] to the set *)
let add_plus1_minus1 set =
  IntSet.fold (fun i set ->
    set
    |> IntSet.add (i-1)
    |> IntSet.add (i+1)) set set

(** No need to scan every coordinate, only lines and columns where points occur (+ or - 1)
    Return them as sorted array for fast iteration *)
let lines_and_columns_indexes input =
  let lines, cols =
    List.fold_left
    (fun (lines, columns) (x,y) -> IntSet.add y lines, IntSet.add x columns)
    (IntSet.empty, IntSet.empty) input in
  lines |> add_plus1_minus1 |> IntSet.elements |> Array.of_list,
  cols |> add_plus1_minus1 |> IntSet.elements |> Array.of_list

(** Remap true indexed to compressed ones *)
let find_pos array x = match array_binary_search (fun pos -> Int.compare pos x) array with
  | Present i -> assert (array.(i) = x); i
  | Absent _ -> failwith "Element not in array"


(** Create a grid initialized as all inside, then run along the perimeter
    marking it as boudary, finally flood-fill from the outside *)
type tile = Inside | Boundary | Outside

let rec mark_boundary grid lines cols = function
  | [] | _::[] -> ()
  | (xa,ya)::((xb,yb)::_ as r) ->
      let y_0 = find_pos lines (min ya yb) in
      let y_max = find_pos lines (max ya yb) in
      for x = find_pos cols (min xa xb) to find_pos cols (max xa xb) do
        for y = y_0 to y_max do
          Grid.set grid (x,y) Boundary
        done;
      done;
      mark_boundary grid lines cols r

let rec flood_fill grid = function
  | [] -> ()
  | x::r when Grid.get grid x != Inside -> flood_fill grid r
  | x::r ->
      Grid.set grid x Outside;
      flood_fill grid (Grid.fold_adjacent
        (fun _ pos elt l -> if elt = Inside then pos::l else l)
        r x grid)

exception Invalid

let valid grid lines cols (xa,ya) (xb,yb) =
    let xa = find_pos cols xa in
    let xb = find_pos cols xb in
    let ya = find_pos lines ya in
    let yb = find_pos lines yb in
    Range.forall (fun y -> Grid.get grid (xa,y) <> Outside &&
                           Grid.get grid (xb,y) <> Outside)
      (Range.inclusive_interval (min ya yb) (max ya yb))
    && Range.forall (fun x -> Grid.get grid (x,ya) <> Outside &&
                            Grid.get grid (x,yb) <> Outside)
      (Range.inclusive_interval (min xa xb) (max xa xb))

let part2 input =
  let lines, cols = lines_and_columns_indexes input in
  let grid = Grid.make (Array.length cols, Array.length lines) Inside in
  mark_boundary grid lines cols (List.hd input :: List.rev input);
  flood_fill grid [(0,0)];
  (* slightly faster to try the largest areas first *)
  let rectangles =
    list_fold_pairs ~reflexive:false
    (fun acc a b -> (area a b, a, b)::acc) [] input in
  let (area, _, _) =
    List.sort (fun (a1, _, _) (a2, _, _) -> - Int.compare a1 a2) rectangles
    |> list_find_first (fun (_, a, b) -> valid grid lines cols a b)
    |> Option.get
    |> fst
  in area

let () = register_int ~year:2025 ~day:09 ~preprocess ~part1 ~part2
