let ( % ) x y =
  let a = x mod y in
  if a < 0 then a + y else a

let rec read_all_lines acc =
  try read_all_lines (read_line ()::acc)
  with End_of_file -> List.rev acc

let read_all_lines () = read_all_lines []

let rec list_assoc_update f n = function
  | [] -> [n, f None]
  | (n', a)::ns when n = n' -> (n, f (Some a))::ns
  | elt::ns -> elt::list_assoc_update f n ns

let array_get_opt array x =
  let n = Array.length array in
  if 0 <= x && x < n
  then Some (Array.unsafe_get array x)
  else None

let set_fold_pairs (type elt set) (module Set: Set.S with type elt = elt and type t = set) f (set: set) acc =
  Set.fold (fun elt acc ->
    match Set.to_seq_from elt set () with
    | Seq.Nil -> acc
    | Seq.Cons(_,s) -> Seq.fold_left (fun acc elt' -> f elt elt' acc) acc s
    ) set acc



type solution = Wrapped: {
  preprocess: string list -> 'a;
  part1: 'a -> string;
  part2: 'a -> string;
} -> solution

let solutions = ref []

let register ~year ~day ~preprocess ~part1 ~part2 =
  if List.mem_assoc (year, day) !solutions then
    failwith ("Duplicate solution for " ^(string_of_int year)^" day "^(string_of_int day));
  solutions := ((year, day), Wrapped{preprocess;part1;part2})::!solutions

let register_int ~year ~day ~preprocess ~part1 ~part2 =
  register ~year ~day ~preprocess
    ~part1:(fun x -> string_of_int (part1 x))
    ~part2:(fun x -> string_of_int (part2 x))


let run_solution ~year ~day =
  match List.assoc (year, day) !solutions with
  | Wrapped{preprocess;part1;part2} ->
      Format.printf "Running puzzle %4d, day %2d@." year day;
      let input = preprocess (read_all_lines ()) in
      Format.printf "Part 1 : %s@." (part1 input);
      Format.printf "Part 2 : %s@." (part2 input)
  | exception Not_found ->
      Format.eprintf "No puzzle solution for %4d, day %2d@." year day
