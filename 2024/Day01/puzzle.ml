(* Puzzle 01 : https://adventofcode.com/2024/day/1 *)

let preprocess input =
  List.split (List.map (fun s -> Scanf.sscanf s "%d   %d" (fun a b -> (a,b))) input)


let rec sum_diff sum l r = match l,r with
  | [], [] -> sum
  | a::l, b::r -> sum_diff (sum + (abs (a-b))) l r
  | _ -> failwith "mismatched lengths"

let part1 (la, lb) =
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

let part2 (la, lb) =
  let occurences = compute_occurences lb in
  List.fold_left (fun acc elt -> acc + elt * match Hashtbl.find occurences elt with
    | n -> n
    | exception Not_found -> 0
  ) 0 la

let () = register_int ~year:2024 ~day:01 ~preprocess ~part1 ~part2
