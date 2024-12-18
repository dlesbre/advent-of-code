(* ==== Puzzle 18 : https://adventofcode.com/2024/day/18 ====  *)

let preprocess input = List.map Vec2.of_string input

let test_size = (6,6)
let true_size = (70,70)
let size () = if !test then test_size else true_size

let rec set_of_binding set n list = match n, list with
  | 0, _ | _, [] -> set
  | n, x::list -> Hashtbl.add set x (); set_of_binding set (n-1) list

let add_elt i n imap =
  IntMap.update i (function
    | None -> Some [n]
    | Some ns -> Some (n::ns))
  imap

let rec bfs seen queue =
  let (score, pos, queue) = imap_pop_minimum queue in
  if not(Vec2.in_box pos ~low:(0,0) ~high:(size ())) then bfs seen queue else
  if Hashtbl.mem seen pos then bfs seen queue
  else
    if pos = size () then score
    else begin
      let open Vec2 in
      Hashtbl.replace seen pos ();
      let score = score + 1 in
      queue |>
        add_elt score (pos +| (1,0)) |>
        add_elt score (pos +| (-1,0)) |>
        add_elt score (pos +| (0,1)) |>
        add_elt score (pos +| (0,-1)) |>
      bfs seen
    end

let shortest_path input n =
  let corrupted = set_of_binding (Hashtbl.create 1000) n input in
  bfs corrupted (IntMap.singleton 0 [0,0])

let part1 input = shortest_path input (if !test then 12 else 1024) |> string_of_int

let part2 input =
  match binary_search (fun n -> match shortest_path input n with
    | _ -> -1
    | exception Not_found -> 1
  ) 0 (List.length input - 1) with
  | Absent n -> Format.asprintf "Block %d - %a" n  Vec2.pp @@ List.nth input (n-1)
  | Present _ -> failwith "Should not happen  "

let () = register ~year:2024 ~day:18 ~preprocess ~part1 ~part2
