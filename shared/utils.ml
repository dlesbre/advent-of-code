let ( % ) x y =
  let y = abs y in
  let a = x mod y in
  if a < 0 then a + y else a

let fail fmtstr = Format.ksprintf failwith fmtstr


let euclid_div x y =
  let y' = abs y in
  let a = x mod y' in
  let res = if a < 0 then (x / y') - 1 else x / y' in
  if y > 0 then res else -res

let rec pow res a = function
  | 0 -> res
  | n -> pow (res*(if n land 1 = 0 then 1 else a)) (a * a) (n / 2)
let pow = pow 1

let string_suffix str start = String.sub str start (String.length str - start)

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

let ( |>> ) f g x = g (f x)

let rec list_sum f total = function
  | [] -> total
  | x::xs -> list_sum f (total + f x) xs
let list_sum f l = list_sum f 0 l

let rec list_count f total = function
  | [] -> total
  | x::xs -> list_count f (total + if f x then 1 else 0) xs
let list_count f l = list_count f 0 l

let list_min f l = List.fold_left (fun m x -> min m (f x)) Int.max_int l

let rec list_find_first pos f = function
  | [] -> None
  | x::_ when f x -> Some (x, pos)
  | _::xs -> list_find_first (pos + 1) f xs
let list_find_first f l = list_find_first 0 f l

let rec list_split f local_acc global_acc = function
  | [] -> List.rev ((List.rev local_acc)::global_acc)
  | x::xs when f x -> list_split f [] (List.rev local_acc::global_acc) xs
  | x::xs -> list_split f (x::local_acc) global_acc xs
let list_split f l = list_split f [] [] l

let list_foldi f acc l = List.fold_left (fun (i, acc) elt -> (i+1, f i acc elt)) (0, acc) l |> snd

let rec list_fold_pairs  ~reflexive f acc = function
  | [] -> acc
  | t::q -> let acc = if reflexive then f acc t t else acc in
            let acc = List.fold_left (fun acc t' -> f acc t t') acc q in
            list_fold_pairs ~reflexive f acc q

let list_pp f fmt = Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ") f fmt

let string_foldi f str acc =
  String.fold_left (fun (i, acc) c -> (i+1,f i c acc)) (0, acc) str |> snd

let string_slice ?(start=0) ?stop string =
  let len = String.length string in
  let stop = Option.value stop ~default:len in
  let start' = if start < 0 then len + start else start in
  let stop' = if stop < 0 then len + stop else stop in
  if start' < 0 || stop' < start' || len < stop'
  then fail "invalid string_slice: [%d:%d] from string of length %d" start stop len
  else String.sub string start' (stop'-start')

let parse_char c = let n = int_of_char c - int_of_char '0' in
  if n < 0 || n > 9 then fail "invalid parse_char: %c" c else n

let set_fold_pairs (type elt set) (module Set: Set.S with type elt = elt and type t = set) f (set: set) acc =
  Set.fold (fun elt acc ->
    match Set.to_seq_from elt set () with
    | Seq.Nil -> acc
    | Seq.Cons(_,s) -> Seq.fold_left (fun acc elt' -> f elt elt' acc) acc s
    ) set acc

let set_sum (type elt set) (module Set: Set.S with type elt = elt and type t = set) f set =
  Set.fold (fun x acc -> acc + f x) set 0

let set_count (type elt set) (module Set: Set.S with type elt = elt and type t = set) f set =
  Set.fold (fun x acc -> if f x then acc + 1 else acc) set 0

let hashtbl_incr table key n =
  match Hashtbl.find table key with
  | x -> Hashtbl.replace table key (x+n)
  | exception Not_found -> Hashtbl.add table key n

let hashtbl_cons table key n =
  match Hashtbl.find table key with
  | x -> Hashtbl.replace table key (n :: x)
  | exception Not_found -> Hashtbl.add table key [n]

type 'a max_n = { size: int; elts: 'a list }
let max_n_init = { size=0; elts=[] }
let rec insert compare elt = function
  | [] -> [elt]
  | (x::_ as list) when compare elt x <= 0 -> elt::list
  | x::q -> x::(insert compare elt q)
let max_n n compare { size; elts } elt =
  let elts = insert compare elt elts in
  if size = n
  then { size; elts=List.tl elts }
  else { size=size+1; elts }

module IntSet = Set.Make(Int)
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

module IntifyOrder(T: Map.OrderedType) = struct
  module M = Map.Make(T)
  let memo = ref M.empty
  let count = ref (-1)

  let get_id x = match M.find x !memo with
    | n -> n
    | exception Not_found ->
        incr count;
        memo := M.add x !count !memo;
        !count
end

module IntifyHash(T: Hashtbl.HashedType) = struct
  module H = Hashtbl.Make(T)
  let table = H.create 10
  let count = ref (-1)
  let get_id x = match H.find table x with
  | n -> n
  | exception Not_found ->
      incr count;
      H.add table x !count;
      !count
end

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

let array_binary_search f array = binary_search (fun i -> f array.(i)) 0 (Array.length array - 1)

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
  part1: 'a -> string * 'b;
  part2: 'a -> 'b -> string;
} -> solution

let solutions = ref []

let register_chained ~year ~day ~preprocess ~part1 ~part2 =
  if List.mem_assoc (year, day) !solutions then
    failwith ("Duplicate solution for " ^(string_of_int year)^" day "^(string_of_int day));
  solutions := ((year, day), Wrapped{ preprocess; part1; part2 })::!solutions

let register_int_chained ~year ~day ~preprocess ~part1 ~part2 =
  register_chained ~year ~day ~preprocess
    ~part1:(fun x -> let res, c = part1 x in string_of_int res, c)
    ~part2:(fun x y -> string_of_int (part2 x y))

let register ~year ~day ~preprocess ~part1 ~part2 =
  register_chained ~year ~day ~preprocess
    ~part1:(fun x -> part1 x, ())
    ~part2:(fun x ()  -> part2 x)

let register_int ~year ~day ~preprocess ~part1 ~part2 =
  register ~year ~day ~preprocess
    ~part1:(fun x -> string_of_int (part1 x))
    ~part2:(fun x -> string_of_int (part2 x))

let run_solution ~year ~day =
  match List.assoc (year, day) !solutions with
  | Wrapped{preprocess;part1;part2} ->
      Format.printf "Running puzzle %4d, day %2d@." year day;
      let input = preprocess (read_all_lines ()) in
      let res, p1 = part1 input in
      Format.printf "Part 1 : %s@." res;
      Format.printf "Part 2 : %s@." (part2 input p1)
  | exception Not_found ->
      Format.eprintf "No puzzle solution for %4d, day %2d@." year day
