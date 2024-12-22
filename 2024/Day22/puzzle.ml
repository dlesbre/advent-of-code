(* ==== Puzzle 22 : https://adventofcode.com/2024/day/22 ====  *)

let preprocess input = List.map int_of_string input
let mix secret x = secret lxor x
let prune number = number mod 16777216
let next_number secret =
  let secret = secret lsl 6 |> mix secret |> prune in
  let secret = secret lsr 5 |> mix secret |> prune in
  secret lsl 11 |> mix secret |> prune

let rec iter number = function
  | 0 -> number
  | n -> iter (next_number number) (n-1)

let part1 = list_sum (fun n -> iter n 2000)

let prices number =
  let nb = ref number in
  let prices = Array.init 2001 (fun _ ->
    let n = !nb in
    nb := next_number n;
    n mod 10
  ) in
  let price_diff = Array.init 2000 (fun i -> prices.(i+1) - prices.(i))
  in prices, price_diff

let sequences global_table (prices, prices_diff) =
  let seen = Hashtbl.create 1000 in
  Range.iter (fun i ->
    let seq = Array.sub prices_diff i 4 in
    if not (Hashtbl.mem seen seq) then (
      hashtbl_incr global_table seq (prices.(i+4));
      Hashtbl.add seen seq ()
    ))
    (Range.upto (2000-3))

let part2 input =
  let global_table = Hashtbl.create 4096 in
  let prices = List.map prices input in
  List.iter (sequences global_table) prices;
  Hashtbl.fold (fun _ -> max) global_table 0

let () = register_int ~year:2024 ~day:22 ~preprocess ~part1 ~part2
