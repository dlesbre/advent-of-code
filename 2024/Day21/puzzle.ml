(* ==== Puzzle 21 : https://adventofcode.com/2024/day/21 ====  *)

let preprocess input = input, Hashtbl.create 4096

let door_keypad = function
  | '7' -> (0,0)
  | '8' -> (1,0)
  | '9' -> (2,0)
  | '4' -> (0,1)
  | '5' -> (1,1)
  | '6' -> (2,1)
  | '1' -> (0,2)
  | '2' -> (1,2)
  | '3' -> (2,2)
  | 'E' -> (0,3)
  | '0' -> (1,3)
  | 'A' -> (2,3)
  | _ -> failwith "Invalid door code"

let robot_keypad =
  let open Grid in function
  | 'E' -> (0,0)
  | '^' -> (1,0)
  | 'A' -> (2,0)
  | '<' -> (0,1)
  | 'v' -> (1,1)
  | '>' -> (2,1)
  | _ -> failwith "Invalid robot code"

(** Create a string representing the path taking n steps in dir, followed
    by n' steps in dir', ending at A *)
let path n dir n' dir' = String.make n dir ^ String.make n' dir' ^ "A"

(** Greedy optimization: we either go all the way up/down, then all the way left/right,
    or all the way left/right, then up/down,
    but we don't interleave up/left/up movement *)
let paths_to (x,y) (x',y') failpos =
  let l =
    if (x',y) <> failpos then
      [path (abs @@ x'-x) (if x' > x then '>' else '<')
            (abs @@ y'-y) (if y' > y then 'v' else '^')]
    else []
  in
  if (x,y') <> failpos then
    path (abs @@ y'-y) (if y' > y then 'v' else '^')
         (abs @@ x'-x) (if x' > x then '>' else '<') :: l
  else l

let rec minimal_presses table depth keypad sequence =
  match Hashtbl.find_opt table (depth, sequence) with
  | Some mini -> mini
  | None ->
      if depth = 0 then String.length sequence else
      let seq, _ = String.fold_left (fun (list,pos) chr ->
        let npos = keypad chr in
        let paths = paths_to pos npos (keypad 'E') in
        paths::list, npos
      ) ([], keypad 'A') sequence in
      let sum = list_sum (list_min (minimal_presses table (depth - 1) robot_keypad)) seq in
      Hashtbl.add table (depth, sequence) sum;
      sum

let numeric_part seq =
  String.to_seq seq |>
  Seq.filter (fun x -> x <> 'A') |>
  String.of_seq |>
  int_of_string

let compute n (input, table) =
  list_sum (fun seq -> minimal_presses table n door_keypad seq * numeric_part seq) input

let part1 = compute 3
let part2 = compute 26

let () = register_int ~year:2024 ~day:21 ~preprocess ~part1 ~part2
