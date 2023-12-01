(* ==== Puzzle 13 : https://adventofcode.com/2022/day/13 ==== *)

(** Returns the suffix of str starting at start *)
let string_suffix str start =
  let len = String.length str in
  if start > len then "" else
  String.sub str start (String.length str - start)

type packet =
  | P_List of packet list
  | P_Int of int

let rec p_compare l r =
  match l, r with
  | P_Int l, P_Int r -> compare l r
  | P_List l, P_List r -> compare_list l r
  | _, P_List r -> compare_list [l] r
  | P_List l, _ -> compare_list l [r]

and compare_list l r =
  match l, r with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | l::ls, r::rs ->
      let c = p_compare l r in
      if c = 0 then compare_list ls rs else c

let ( <== ) l r = p_compare l r <= 0

let rec pp_packet fmt = function
  | P_Int d -> Format.pp_print_int fmt d
  | P_List l -> Format.fprintf fmt "[%a]" (
      Format.pp_print_list
        ~pp_sep:(fun f x -> Format.fprintf f ",")
        pp_packet) l

exception Found of int

let rec find_closing line =
  try
    let _ = String.fold_left (fun (depth, pos) c ->
        let depth = match c with
        | '[' -> depth+1
        | ']' -> depth-1
        | _ -> depth
        in
        if depth = 0 then raise (Found pos);
        (depth, pos+1)
      ) (0,0) line in
    failwith "Not found"
  with Found pos -> pos

let find_first iteri chr string =
  try
    let _ = iteri (fun i c -> if c = chr then raise (Found i)) string in None
  with Found p -> Some p

let rec read_packet line =
  if line.[0] = '[' then
    let close = find_closing line in
    assert(close = String.length line - 1);
    let line = String.sub line 1 (String.length line - 2) in
    P_List(read_packet_list line)
  else
    P_Int (int_of_string line)

and read_packet_list line =
  if line = "" then [] else
  if line.[0] = '[' then
    let close = find_closing line+1 in
    let packet = read_packet (String.sub line 0 close) in
    packet::(read_packet_list (string_suffix line (close+1)))
  else
    match find_first String.iteri ',' line with
    | None -> [read_packet line]
    | Some x ->
      read_packet (String.sub line 0 x)::(read_packet_list (string_suffix line (x+1)))

let rec read_packet_pairs acc =

  let packet1 = read_packet (read_line ()) in
  let packet2 = read_packet (read_line ()) in
  let acc = (packet1, packet2)::acc in
  try
    let _ = read_line () in
    read_packet_pairs acc
  with End_of_file ->
    List.rev acc

let part1 () =
  let packets = read_packet_pairs [] in
  let _, sum = List.fold_left (fun (i, acc) (l,r) ->
      if l <== r then (i+1, acc+i) else (i+1, acc)
    ) (1,0) packets in
  Format.printf "%d\n" sum



let part2 () =
  let div_pack_1 = P_List [P_List [P_Int 2]] in
  let div_pack_2 = P_List [P_List [P_Int 6]] in
  let packets = read_packet_pairs [] in
  let packets = List.flatten (List.map (fun (x,y) -> [x;y]) (packets)) in
  let packets = div_pack_1 :: div_pack_2 :: packets in
  let packets = List.sort (p_compare) packets in
  let pack1 = find_first List.iteri div_pack_1 packets in
  let pack2 = find_first List.iteri div_pack_2 packets in
  Format.printf "%d\n"
    ((Option.get pack1 + 1) * (Option.get pack2 + 1))

let () = part2 ()
