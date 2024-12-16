(* ==== Puzzle 09 : https://adventofcode.com/2024/day/9 ====  *)

type content =
  | Empty of int
  | File of int * int (** id, size *)

let int_of_char x = int_of_char x - int_of_char '0'

let parse line =
  Array.init (String.length line) (fun i ->
      if i mod 2 = 0
      then File (i / 2, int_of_char line.[i])
      else Empty (int_of_char line.[i])
    )

(** Sum of integers in [low..high] inclusive *)
let int_sum low high = (high-low+1) * (high+low) / 2

(** We iterate from the ends towards the center
    - [left] is the position of the left cursor the array, [right] that of the right cursor
    - [pos] is the position from the start of the filesystem (number of blocks)*)
let rec part1 array total left pos right =
  if left >= right then
    match array.(left) with
    | Empty _ -> total
    | File (id, size) -> part1 array (total + id*(int_sum pos (pos+size-1)))
    (left+1) (pos+size) right
  else
    match array.(left) with
    | File(id, size) ->
        part1 array (total + id*(int_sum pos (pos+size-1)))
          (left+1) (pos+size) right
    | Empty size ->
        match array.(right) with
        | Empty _ ->
            part1 array total left pos (right-1)
        | File(id, size') ->
            if size >= size' then (
              array.(left) <- Empty (size-size');
              part1 array (total + id*(int_sum pos (pos+size'-1))) left (pos+size') (right-1)
            )
            else (
              array.(right) <- File(id, size'-size);
              part1 array (total + id*(int_sum pos (pos+size-1))) (left+1) (pos+size) right
            )

let size = function
  | Empty size -> size
  | File (_, size) -> size

(** Precomputed filesystem position for each element of the old array
    ex: [positions [|File(_,3); Empty 5; File _]|] = [|0;3;3+5|]]  *)
let positions array =
  let total = ref 0 in
  Array.init (Array.length array) (fun i ->
    let tmp = !total in
    total := !total + size array.(i);
    tmp)

(** Array fold_right with position*)
let array_fold_right_i f array acc =
  Array.fold_right (fun elt (acc,pos) ->
    (f elt pos acc, pos-1)
    ) array (acc, Array.length array - 1) |> fst

let part2 array =
  (* find an empty spot with sufficient size before max_pos, starting at pos *)
  let rec find_empty size max_pos pos =
    if pos > max_pos
    then None
    else match array.(pos) with
    | File _ -> failwith "Only empty at odd positions"
    | Empty n when n >= size ->
          array.(pos) <- Empty (n-size); (* Shrink the free block *)
          Some pos
    | Empty _ -> find_empty size max_pos (pos+2) (* Empty are at odd positions *)
  in
  (* Memoization: empty[i] is the first empty that MAY fit a block of size i. *)
  let emptys = Array.make 10 (Some 1) in (* start at odd positions *)
  let find_empty size max_pos = match emptys.(size) with
    | None -> None
    | Some pos -> let opt = find_empty size max_pos pos in
                  emptys.(size) <- opt;
                  opt
  in
  let positions = positions array in
  array_fold_right_i (fun elt i total -> match elt with
    | Empty _ -> total
    | File(id, size) ->
        let new_pos = match find_empty size i with
        | None -> positions.(i)
        | Some i -> let n = positions.(i) in
                    positions.(i) <- positions.(i) + size;
                    n
        in total + id*(int_sum new_pos (new_pos+size-1))
  ) array 0

let preprocess lines = parse (List.hd lines)
let part1 array = part1 (Array.copy array) 0 0 0 (Array.length array -1)

let () = register_int ~year:2024 ~day:09 ~preprocess ~part1 ~part2
