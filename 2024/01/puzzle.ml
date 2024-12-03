(* Puzzle 01 : https://adventofcode.com/2024/day/1 *)

let read_input () =
  let rec aux l r =
    match read_line () with
    | s ->
        Scanf.sscanf s "%d   %d" (fun a b -> aux (a::l) (b::r))
    | exception End_of_file -> l, r
  in aux [] []

let rec sum_diff sum l r = match l,r with
  | [], [] -> sum
  | a::l, b::r -> sum_diff (sum + (abs (a-b))) l r
  | _ -> failwith "mismatched lengths"

let part1 la lb =
  let la = List.sort Int.compare la in
  let lb = List.sort Int.compare lb in
  sum_diff 0 la lb

let compute_occurences list =
  let occurences = Hashtbl.create 100 in
  let rec aux = function
    | [] -> occurences
    | x::xs ->
        (if Hashtbl.mem occurences x
        then Hashtbl.replace occurences x (Hashtbl.find occurences x + 1)
        else Hashtbl.add occurences x 1
        );
        aux xs
  in aux list

let part2 la lb =
  let occurences = compute_occurences lb in
  List.fold_left (fun acc elt -> acc + elt * match Hashtbl.find occurences elt with
    | n -> n
    | exception Not_found -> 0
  ) 0 la

let main () =
  let l, r = read_input () in
  Format.printf "Part 1 : %d@." (part1 l r);
  Format.printf "Part 1 : %d@." (part2 l r)

let () = main()
