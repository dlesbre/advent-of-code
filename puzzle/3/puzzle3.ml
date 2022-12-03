let code_a = int_of_char 'a'
let code_z = int_of_char 'z'
let code_A = int_of_char 'A'
let code_Z = int_of_char 'Z'

let priority item =
  let code = int_of_char item in
  if code_a <= code && code <= code_z then code - code_a + 1
  else if code_A <= code && code <= code_Z then code - code_A + 27
  else failwith "Not a letter"

module CharSet = Set.Make(struct
  type t = char
  let compare = compare
end)

let items_in_both contents =
  let length = String.length contents in
  let half_length = length / 2 in
  let rec runner left right = function
    | -1 -> left, right
    | i ->
      let left = CharSet.add contents.[i] left in
      let right = CharSet.add contents.[i+half_length] right in
      runner left right (i-1) in
  let left, right = runner CharSet.empty CharSet.empty (half_length-1) in
  CharSet.inter left right, CharSet.union left right

let item_priorities set =
  CharSet.fold
    (fun chr (total,number) -> (total + priority chr, number + 1)) set (0,0)

let rec get_rucksacks acc =
  try
    let line = read_line () in
    get_rucksacks ((items_in_both line)::acc)
  with End_of_file -> acc

let part1 () =
  let conflicts = get_rucksacks [] in
  let weights = List.map (fun x -> item_priorities (fst x)) conflicts in
  let total = List.fold_left (fun total (priority, number) -> assert(number=1); total+priority) 0 weights in
  Format.printf "%d\n" total

(* ==== Part 2 ==== *)

let rec fold_list acc items list =
  let acc, items = match items with
    | [a;b;c] ->
        let set = CharSet.inter a (CharSet.inter b c) in
        let priorities, number = item_priorities set in
        assert(number = 1); priorities + acc, []
    | _ -> acc, items
  in
  match list with
  | [] -> assert(list=[]); acc
  | (_,t)::q -> fold_list acc (t::items) q


let part2 () =
  let conflicts = get_rucksacks [] in
  let weight = fold_list 0 [] conflicts in
  Format.printf "%d\n" weight

let () = part2 ()
