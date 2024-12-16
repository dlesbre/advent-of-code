(* ==== Puzzle 05 : https://adventofcode.com/2024/day/5 ====  *)

module HashtblNotation = struct
  let ( .%[] ) = Hashtbl.find
  let ( .%?[] ) = Hashtbl.find_opt
  let ( .%[] <- ) = Hashtbl.replace
end


let rec parse pairs = function
  | ""::remaining_lines -> pairs, List.map (fun x -> String.split_on_char ',' x |> List.map int_of_string) remaining_lines
  | line::remaining_lines -> parse ((Scanf.sscanf line "%d|%d" (fun x y -> (x,y)))::pairs) remaining_lines
  | _ -> failwith "Invalid format"

open HashtblNotation

(** Create a hashmap nb -> set of pages bigger than nb
    Where the set is represented by a hashmap to unit *)
let create_ordering list =
  let hashtable = Hashtbl.create 100 in
  let rec runner = function
    | [] -> ()
    | (x,y)::rest ->
        begin match hashtable.%?[x] with
        | Some table -> table.%[y] <- ()
        | None ->
            let table = Hashtbl.create 10 in
            table.%[y] <- ();
            hashtable.%[x] <- table
        end;
        runner rest
  in runner list;
  hashtable

let pp_tb = Hashtbl.iter (fun x table ->
  Format.printf "  %d -> " x;
  Hashtbl.iter (fun y () -> Format.printf "%d," y) table;
  Format.printf "@."
  )

let rec well_ordered ordering_table = function
  | [] -> true
  | x::rest ->
      List.for_all (fun y ->
        match ordering_table.%?[y] with
        | None -> true
        | Some table -> not (Hashtbl.mem table x)
      ) rest
      && well_ordered ordering_table rest

let sum_middles list =
  List.fold_left (fun total lst ->
    let l = List.length lst in
    let _ = assert (l mod 2 == 1) in
    let middle = List.nth lst (l / 2) in
    total + middle
    ) 0 list

exception Sorted

let rec make_well_ordered ordering_table = function
  | [] -> []
  | x::rest ->
      (* Inefficient sort, sure, but the lists are small enough it hardly matters *)
      let rec sorter = function
        | [] -> raise Sorted
        | y::r ->
        match ordering_table.%?[y] with
        | Some table when (Hashtbl.mem table x) -> x::r, y
        | _ -> let (l, v) = sorter r in y::l, v
      in match sorter rest with
      | rest', v -> make_well_ordered ordering_table (v ::rest')
      | exception Sorted -> x::(make_well_ordered ordering_table rest)

let preprocess lines =
  let ordering_rules, page_numbers = parse [] lines in
  let ordering_table = create_ordering ordering_rules in
  ordering_table, List.partition (well_ordered ordering_table) page_numbers

let part1 (_, (well_ordered, _)) = sum_middles well_ordered
let part2 (ordering_table, (_, unordered)) = sum_middles (List.map (make_well_ordered ordering_table) unordered)

let () = register_int ~year:2024 ~day:05 ~preprocess ~part1 ~part2
