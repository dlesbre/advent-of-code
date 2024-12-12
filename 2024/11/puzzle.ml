(* ==== Puzzle 11 : https://adventofcode.com/2024/day/11 ====  *)

let input () =
  read_line ()
  |> String.split_on_char ' '
  |> List.map int_of_string

let rec nb_digits n candidate nb =
  if candidate > n
  then nb
  else nb_digits n (candidate*10) (nb+1)
let nb_digits n = nb_digits n 10 1

let rec exp a b r =
  if b == 0 then r
  else exp (a*a) (b lsr 1) (if b land 1 == 1 then r*a else r)
let exp a b = exp a b 1

(** Take one step, for one stone *)
let step stone =
  if stone == 0 then [1]
  else
    let digi = nb_digits stone in
    if digi land 1 = 0 then (* even *)
      let split = exp 10 (digi lsr 1) in
      [ stone / split; stone mod split ]
    else
      [ stone*2024 ]

(** [table stone] is a list, where the i-th element is the
    number of stones after [i] steps starting from the given [stone].
    (if the list is too short, we recompute it) *)
let table = Hashtbl.create 4096

let pp fmt l = Format.fprintf fmt "[%a]" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ") Format.pp_print_int) l

(** Merge list from children into parent list (by pointwise sums) *)
let[@tail_mod_cons] rec merge_lists n next =
  if n < 0 then [] else
    List.fold_left (fun acc x -> acc + List.hd x) 0 next ::
    merge_lists (n-1) (List.map List.tl next)

(** Compute a list of length n, where the ith element is the number of stones
    after i steps, starting from the given stone.
    Memoization is essential for performance here. *)
let rec compute_list n stone =
  match Hashtbl.find_opt table stone with
  | Some l when List.length l > n -> l
  | _ ->
    if n == 1 then [1; List.length (step stone)]
    else
      let next = List.map (compute_list (n-1)) (step stone) in
      let result = 1 :: merge_lists (n-1) next in
      Hashtbl.replace table stone result;
      result

let total n input =
  List.map (fun stone -> List.nth (compute_list n stone) n) input |>
  List.fold_left (+) 0

let main () =
  let input = input () in
  Format.printf "Part 1 : %d@." (total 25 input);
  Format.printf "Part 2 : %d@." (total 75 input)

let () = main ()
