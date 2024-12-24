(* ==== Puzzle 24 : https://adventofcode.com/2024/day/24 ====  *)

let parse_wire = function
  | 0 -> false
  | 1 -> true
  | _ -> failwith "Invalid format"

let parse_op op = match op with
  | "AND" -> ( && )
  | "OR" -> ( || )
  | "XOR" -> (fun x y -> if x then not y else y)
  | _ -> failwith "Invalid operator"

let parse_gate table str =
  Scanf.sscanf str "%s %s %s -> %s" (fun left op right result ->
      Hashtbl.add table result (left, right, parse_op op, op))

let preprocess input =
  let rec initial_values acc = function
    | ""::gates -> acc, gates
    | s::rest ->
        let var, value = Scanf.sscanf s "%s %d" (fun s d -> String.sub s 0 (String.length s - 1), parse_wire d) in
        Hashtbl.add acc var value;
        initial_values acc rest
    | [] -> failwith "Invalide format"
  in let initial_values, gates = initial_values (Hashtbl.create 1000) input in
  let table = Hashtbl.create 4096 in
  List.iter (parse_gate table) gates;
  initial_values, table

(** Topological ordering of values to compute, excluding the initial values *)
let topological_sort initial table =
  let seen = Hashtbl.create 1000 in
  Hashtbl.iter (fun a _ -> Hashtbl.add seen a ()) initial;
  let ordering = ref [] in
  let rec insert_in_ordering node =
    let (l,r,_,_) =  Hashtbl.find table node in
    if not (Hashtbl.mem seen l) then insert_in_ordering l;
    if not (Hashtbl.mem seen r) then insert_in_ordering r;
    ordering := node :: !ordering;
    Hashtbl.add seen node ()
  in
  Hashtbl.iter (fun x _ -> insert_in_ordering x) table;
  List.rev !ordering

let get_binary_value values prefix =
  Hashtbl.fold (fun k v list -> if k.[0] = prefix then (k,v)::list else list) values [] |>
  List.sort (fun (a,_) (b,_) -> - String.compare a b) |>
  List.fold_left (fun i (_,b) -> if b then i lsl 1 lor 1 else i lsl 1) 0

let part1 (values, table) =
  let order = topological_sort values table in
  List.iter (fun x ->
    let (l,r,op,_) = Hashtbl.find table x in
    Hashtbl.add values x (op (Hashtbl.find values l) (Hashtbl.find values r))
  ) order;
  get_binary_value values 'z' |> string_of_int

(** Name some simple values *)
let smart_renames table x =
    if x.[0] = 'z' then None else
    match Hashtbl.find table x with
    | (l, r, _, op) ->
        begin match op with
        | "XOR" when l.[1] = r.[1] && l.[2] = r.[2] && ((l.[0] = 'x' && r.[0] = 'y') || (l.[0] = 'y' && r.[0] = 'x')) ->
                           Some ("xor" ^ (String.sub l 1 2))
        | "AND" when l.[1] = r.[1] && l.[2] = r.[2] && ((l.[0] = 'x' && r.[0] = 'y') || (l.[0] = 'y' && r.[0] = 'x')) ->
                           Some ("and" ^ (String.sub l 1 2))
        | _ -> None
        end
    | exception Not_found -> None


let rec print_deps printed x table =
  if Hashtbl.mem printed x then () else
  match Hashtbl.find table x with
  | (l, r, _, op) ->
      let l' = smart_renames table l in
      let r' = smart_renames table r in
      let pl = match l' with Some l' -> l'^"-"^l | None -> l in
      let pr = match r' with Some r' -> r'^"-"^r | None -> r in
      Format.printf "%s %s %s -> %s@." pl op pr x;
      if Option.is_none l' then print_deps printed l table;
      if Option.is_none r' then print_deps printed r table;
      Hashtbl.add printed x ();
  | exception Not_found -> ()

let swap table (a, b) =
  let b' = Hashtbl.find table a in
  let a' = Hashtbl.find table b in
  Hashtbl.replace table a a';
  Hashtbl.replace table b b'

(** Part2 requires manual work:
    this prints all dependencies of variables zXX in order, skipping
    previously printed values.
    This makes it easy to see where the pattern is broken. All that remains then
    to find the correct swap to perform and add it to the swaps list *)
let part2 (_values, table) =
  let swaps = [("z05", "gdd"); ("z09", "cwt"); ("jmv", "css"); ("pqt", "z37")] in
  List.iter (swap table) swaps;
  let printed = Hashtbl.create 100 in
  let boundary = if !test then 9 else 46 in
  for k = 0 to boundary do
    print_deps printed (Format.sprintf "z%02d" k) table;
    Format.printf "@."
  done;
  let a, b = List.split swaps in a @ b |>
  List.sort String.compare |>
  String.concat ","


let () = register ~year:2024 ~day:24 ~preprocess ~part1 ~part2
