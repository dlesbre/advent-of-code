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

let get b n = int_of_char (Bytes.unsafe_get b n)

let prices number =
  let nb = ref number in
  let prices = Bytes.init 2001 (fun _ ->
    let n = !nb in
    nb := next_number n;
    char_of_int (n mod 10)
  ) in
  let price_diff = Bytes.init 2000 (fun i -> (get prices (i+1)) - (get prices i) + 9 |> char_of_int)
  in prices, price_diff

let sequences global_table (prices, prices_diff) =
  let seen = Hashtbl.create 1000 in
  (* We can represent a numbers between -9 and 9 with 5 bits,
     so a sequence of four can be encoded in 20 bits, a single integer *)
  let sequence =
    (get prices_diff 0 lsl 15) lor
    (get prices_diff 1 lsl 10) lor
    (get prices_diff 2 lsl 5) in
  let sequence = ref sequence in
  Range.iter (fun i ->
    let seq = !sequence lor get prices_diff i in
    if not (Hashtbl.mem seen seq) then (
      hashtbl_incr global_table seq (get prices (i+1));
      Hashtbl.add seen seq ()
    );
    sequence := (seq lsl 5) land 0b11111_11111_11111_11111
    )
    (Range.interval 3 2000)

let part2 input =
  let global_table = Hashtbl.create 4096 in
  let prices = List.map prices input in
  List.iter (sequences global_table) prices;
  Hashtbl.fold (fun _ -> max) global_table 0

let () = register_int ~year:2024 ~day:22 ~preprocess ~part1 ~part2
