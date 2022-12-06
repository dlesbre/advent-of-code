let is_marker a b c d =
  match a, b, c, d with
  | Some a, Some b, Some c, d ->
      not (a = b || a = c || a = d || b = c || b = d || c = d)
  | _ -> false

exception Found of int

let find_marker line =
  try
    let _ = String.fold_left (fun (i,a,b,c) chr ->
      if is_marker a b c chr
      then raise (Found i)
      else (i+1,b,c,Some chr)
    ) (1, None, None, None) line in
    failwith "No marker"
  with Found i -> i

let rec part1 () =
  try
    let line = read_line () in
    let marker = find_marker line in
    Format.printf "Marker at %d\n" marker;
    part1 ()
  with End_of_file -> ()

(* Part 2: the better way *)

module CharSet = Set.Make(struct type t = char let compare = compare end)

let distinct_between str start len =
  let sub = String.sub str start len in
  let chars = String.fold_left (fun s c -> CharSet.add c s) CharSet.empty sub in
  CharSet.cardinal chars = len

let find_first_n_distinct n line =
  let last_possible = String.length line - n in
  let rec runner k =
    if k > last_possible then failwith "Not found"
    else if distinct_between line k n then k+n
    else runner (k+1)
  in runner 0

let rec part2 () =
  let n = 14 in
  try
    let line = read_line () in
    let marker = find_first_n_distinct n line in
    Format.printf "Marker at %d\n" marker;
    part2 ()
  with End_of_file -> ()

let () = part2 ()
