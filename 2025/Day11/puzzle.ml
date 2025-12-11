(* ==== Puzzle 11 : https://adventofcode.com/2025/day/11 ====  *)

let parse_line map line =
  let node = string_slice ~stop:3 line in
  let neighbors = string_slice ~start:5 line
                |> String.split_on_char ' ' in
  StringMap.add node neighbors map

let preprocess = List.fold_left parse_line StringMap.empty

let rec count_paths_1 memo map pos = match Hashtbl.find_opt memo pos with
  | Some n -> n
  | None ->
      if pos = "out" then 1 else
      let neighbors = StringMap.find pos map in
      let n = list_sum (count_paths_1 memo map) neighbors in
      Hashtbl.add memo pos n;
      n

let part1 map =
  let memo = Hashtbl.create 100 in
  count_paths_1 memo map "you"

let rec count_paths_2 memo map visited_dac visited_fft pos =
  let acc = (pos, visited_dac, visited_fft) in
  match Hashtbl.find_opt memo acc with
  | Some n -> n
  | None ->
      let visited_dac = visited_dac || pos = "dac" in
      let visited_fft = visited_fft || pos = "fft" in
      if pos = "out" then Bool.to_int (visited_dac && visited_fft) else
      let neighbors = StringMap.find pos map in
      let n = list_sum (count_paths_2 memo map visited_dac visited_fft) neighbors in
      Hashtbl.add memo acc n;
      n

let part2 map =
  let memo = Hashtbl.create 100 in
  count_paths_2 memo map false false "svr"

let () = register_int ~year:2025 ~day:11 ~preprocess ~part1 ~part2
