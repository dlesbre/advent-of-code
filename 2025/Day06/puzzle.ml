(* ==== Puzzle 06 : https://adventofcode.com/2025/day/06 ====  *)

type 'a operation = {
  numbers: 'a;
  is_addition: bool;
}

let find_breaks grid =
  let lines = Range.upto (Grid.lines grid) in
  let columns = (Grid.columns grid) in
  let breaks = Range.fold (fun col breaks ->
    if Range.forall (fun line -> Grid.get grid (col,line) = ' ') lines
    then col::breaks
    else breaks)
  (Range.upto columns) [] in
  List.rev (columns::breaks) (* add columns to have a virtual last break *)

let rec split_on_breaks lines grid start = function
  | [] -> []
  | stop::next ->
      let start = start + 1 in
      let subgrid = Grid.init (stop - start, lines) (fun (i,j) -> Grid.get grid (i+start,j)) in
      let next = split_on_breaks lines grid stop next in
      { numbers=subgrid; is_addition = Grid.get grid (start, lines) = '+'} :: next
let split_on_breaks grid = split_on_breaks (Grid.lines grid - 1) grid (-1)

let preprocess input =
  let grid = Grid.parse (fun _ c -> c) input in
  let breaks = find_breaks grid in
  split_on_breaks grid breaks

let to_int_list_p1 grid =
  let columns = Range.upto (Grid.columns grid) in
  Range.fold (fun line numbers ->
    let nb = Range.fold (fun col str ->
      let chr = Grid.get grid (col, line) in
      if chr = ' ' then str else str ^ String.make 1 chr) columns ""
    in int_of_string nb :: numbers)
    (Range.upto (Grid.lines grid)) []

(* Same as p1, just flip line and columns *)
let to_int_list_p2 grid =
  let lines = Range.upto (Grid.lines grid) in
  Range.fold (fun col numbers ->
    let nb = Range.fold (fun line str ->
      let chr = Grid.get grid (col, line) in
      if chr = ' ' then str else str ^ String.make 1 chr) lines ""
    in int_of_string nb :: numbers)
    (Range.upto (Grid.columns grid)) []

let operation_map f { numbers; is_addition } = { numbers=f numbers; is_addition }

let perform_operation { numbers; is_addition } =
  if is_addition
  then list_sum Fun.id numbers
  else List.fold_left ( * ) 1 numbers

let part1 grids = List.map (operation_map to_int_list_p1) grids |> list_sum perform_operation
let part2 grids = List.map (operation_map to_int_list_p2) grids |> list_sum perform_operation

let () = register_int ~year:2025 ~day:06 ~preprocess ~part1 ~part2
