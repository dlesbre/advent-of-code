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

module IntMap = Map.Make(Int)

let rec imap_pop_minimum imap = match IntMap.min_binding imap with
  | (i, []) -> imap_pop_minimum (IntMap.remove i imap)
  | (i, [n]) -> i, n, IntMap.remove i imap
  | (i, n::ns) -> i, n, IntMap.add i ns imap

let imap_add_elt i n imap =
  IntMap.update i (function
    | None -> Some [n]
    | Some ns -> Some (n::ns))
  imap

let imap_merge_elt i n f imap =
  IntMap.update i (function
    | None -> Some [(n, f None)]
    | Some ns -> Some (list_assoc_update f n ns))
  imap

type binary_search_result =
  | Present of int
  | Absent of int

let rec binary_search f low high =
  if high < low then Absent low
  else
  let mid = low + (high - low) / 2 in (* does not overflow, unlike (high+low)/2 *)
  let n = f mid in
  if n = 0 then Present mid
  else if n < 0
  then binary_search f (mid+1) high
  else binary_search f low (mid-1)

let rec gcd a = function
  | 0 -> a
  | b -> gcd b (a mod b)
let gcd a b = gcd (abs a) (abs b)

let lcm a b = if a = 0 || b = 0 then 0 else abs (a / gcd a b * b)

let rec bezout_coefficients old_u u old_v v a = function
  | 0 -> (a, old_u, old_v)
  | b ->
      let q = a / b in
      bezout_coefficients u (old_u - q*u) v (old_v-q*v) b (a-q*b)
let bezout_coefficients a b =
  let (gcd, u, v) = bezout_coefficients 1 0 0 1 (abs a) (abs b) in
  gcd, (if a < 0 then -u else u), (if b < 0 then -v else v)

(* Shoelace formula:
   2*A = sum (for i = 1 to N) x_i * (y_[i+1] - y_[i-1])
   Note that y_[i+1] / y_[i-1] may wrap around, we solve this by re-adding
   the last two elements at the front of the list
*)
let rec last_two = function
  | [a;b] -> a,b
  | [] | [_] -> failwith "list too short"
  | _::rest -> last_two rest


let polygon_double_area polygon =
  let a, b = last_two polygon in
  let rec aux sum = function
    | (_,y)::((x,_)::(_,y')::_ as rest) -> aux (sum + x*(y'-y)) rest
    | _ -> sum
  in aux 0 (a::b::polygon)

let test = ref false

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
