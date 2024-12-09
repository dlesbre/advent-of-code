(* ==== Puzzle 09 : https://adventofcode.com/2024/day/9 ====  *)

type content =
  | Empty of int
  | File of int * int

let int_of_char x = int_of_char x - int_of_char '0'

let parse line =
  Array.init (String.length line) (fun i ->
      if i mod 2 = 0
      then File (i / 2, int_of_char line.[i])
      else Empty (int_of_char line.[i])
    )

(** Sum of integers in [low..high] inclusive *)
let int_sum low high =(high-low+1) * (high+low) / 2
  (* let n = (high-low+1) * (high+low) / 2 in
  Format.printf "        Sum %d..%d = %d@." low high n;
  n *)

(** We iterate from the ends towards the center
    - [left] is the position of the left cursor the array, [right] that of the right cursor
    - [pos] is the position from the start of the filesystem (number of blocks)
    - [remain] is non-zero if the current right chunk is only partially consumed *)
let rec part1 array total left pos right =
  if left >= right then
    match array.(left) with
    | Empty _ -> total
    | File (id, size) -> part1 array (total + id*(int_sum pos (pos+size-1)))
    (left+1) (pos+size) right
  else
    match array.(left) with
    | File(id, size) ->
      (* Format.printf "%d => Left file %d,%d@." total id size; *)
        part1 array (total + id*(int_sum pos (pos+size-1)))
          (left+1) (pos+size) right
    | Empty size ->
        (* Format.printf "%d => Left Empty %d@." total  size; *)
        match array.(right) with
        | Empty _ ->
            (* Format.printf "    Right Empty@."; *)
            part1 array total left pos (right-1)
        | File(id, size') ->
            (* Format.printf "    Right File %d,%d@." id size'; *)
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

let positions array =
  let total = ref 0 in
  Array.init (Array.length array) (fun i ->
    let tmp = !total in
    total := !total + size array.(i);
    tmp
    )

(** Array fold_right with position*)
let array_fold_right_i f array acc =
  Array.fold_right (fun elt (acc,pos) ->
    (f elt pos acc, pos-1)
    ) array (acc, Array.length array - 1) |> fst

let part2 array =
  (* find an empty spot with the correct size before max_pos, starting at pos *)
  let rec find_empty size max_pos pos =
    if pos > max_pos
    then None
    else match array.(pos) with
    | File _ -> failwith "Only empty at odd positions"
    | Empty n when n >= size ->
          array.(pos) <- Empty (n-size);
          Some pos
    | Empty _ -> find_empty size max_pos (pos+2)
  in
  (* Memoization: empty[i] is the first empty that MAY fit a block of size i. *)
  let emptys = Array.make 10 (Some 1) in
  let find_empty size max_pos = match emptys.(size) with
    | None -> None
    | Some pos -> let opt = find_empty size max_pos pos in
                  emptys.(size) <- opt;
                  opt
  in
  let positions = positions array in
  array_fold_right_i (fun elt pos total -> match elt with
    | Empty _ -> total
    | File(id, size) ->
        let pos = match find_empty size pos with
        | None -> positions.(pos)
        | Some i -> let n = positions.(i) in
                    positions.(i) <- positions.(i) + size;
                    n
        in total + id*(int_sum pos (pos+size-1))
  ) array 0



let main () =
  let line = read_line () in
  let array = parse line in
  let p1 = part1 (Array.copy array) 0 0 0 (Array.length array -1) in
  Format.printf "Part 1 : %d@." p1;
  Format.printf "Part 2 : %d@." (part2 array)


let () = main ()
