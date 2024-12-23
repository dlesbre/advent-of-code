(* ==== Puzzle 23 : https://adventofcode.com/2024/day/23 ====  *)

(** Transform a two character string into a single 16-bit int *)
let intify s =
  assert (String.length s = 2);
  ((int_of_char s.[0]) lsl 8) lor (int_of_char s.[1])

let starts_with_t int_repr = int_repr lsr 8 = int_of_char 't'

let de_intify x =
  let s = Bytes.of_string "  " in
  Bytes.set s 0 @@ char_of_int (x lsr 8);
  Bytes.set s 1 @@ char_of_int (x land 0xff);
  String.of_bytes s

(** Build an adjacency map x -> connected computers with id greater than x *)
let rec build_graph neighbors = function
  | [] -> neighbors
  | (a,b)::rest ->
      let (x,y) = if a < b then (b,a) else (a,b) in
      begin match Hashtbl.find neighbors y with
      | set -> Hashtbl.replace neighbors y (IntSet.add x set)
      | exception Not_found -> Hashtbl.add neighbors y (IntSet.singleton x)
      end;
      build_graph neighbors rest

let preprocess input =
  List.map (fun x ->
    match String.split_on_char '-' x with
    | [a;b] -> (intify a, intify b)
    | _ -> failwith "Invalid format")
  input
  |> build_graph (Hashtbl.create 4096)

let get_neighbors neighbors j = try Hashtbl.find neighbors j with Not_found -> IntSet.empty

(** Since elements are sorted (i.e. each points only keeps as neighbors those
    larger than it), we can find ordered triples uniquely by iterating the adjacency
    list, with no risk of redundancy. *)
let part1 neighbors =
  Hashtbl.fold (fun x connected_to_x acc ->
    set_sum (module IntSet) (fun j ->
        let inter = IntSet.inter connected_to_x (get_neighbors neighbors j) in
        if starts_with_t x || starts_with_t j
        then IntSet.cardinal inter
        else set_count (module IntSet) starts_with_t inter
      ) connected_to_x + acc
    ) neighbors 0
  |> string_of_int

(** find the largest set of points that are:
    - larger than [x]
    - connected to [x]
    - connected to each other
    Note that this might fail on some inputs, because we assume such a set is unique,
    but it might not be.

    Use memoization to avoid recomputing the same sets again and again. *)
let rec largest_strongly_connected_comp memo neighbors x =
  match Hashtbl.find memo x with
  | set -> set
  | exception Not_found ->
      let connected_to_x = get_neighbors neighbors x in
      let set = IntSet.fold (fun i (set, card) ->
          let (compo,_) = largest_strongly_connected_comp memo neighbors i in
          let set' = IntSet.inter compo connected_to_x |> IntSet.add x in
          let card' = IntSet.cardinal set' in
          if card' > card then (set', card')
          else (set, card))
        connected_to_x
        (IntSet.singleton x, 1)
      in Hashtbl.add memo x set; set

let part2 neighbors =
  let memo = Hashtbl.create 4096 in
  let (set, _) = Hashtbl.fold (fun x _ (maxi, card) ->
    let (comp, card') = largest_strongly_connected_comp memo neighbors x in
    if card' > card then (comp, card') else (maxi, card)
    ) neighbors (IntSet.empty, 0) in
  (* Convert the set to a comma separated sorted string *)
  IntSet.elements set |>
  List.map de_intify |>
  List.sort String.compare |>
  String.concat ","


let () = register ~year:2024 ~day:23 ~preprocess ~part1 ~part2
