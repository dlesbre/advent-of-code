module ListWithAcc(A : sig
  type elem
  type acc
  val initial : acc
  val accumulator : elem -> acc -> acc
end) = struct
  type t = {
    total : A.acc;
    items : A.elem list;
  }

  let empty = {
    total = A.initial;
    items = [];
  }

  let add_item list item = {
    total = A.accumulator item list.total;
    items = item::(list.items);
  }
end

module SumList = ListWithAcc(struct
  type elem = int
  type acc = int
  let initial = 0
  let accumulator = ( + )
end)

module MaxList = ListWithAcc(struct
  type elem = SumList.t
  type acc = int * int (* total, pos *)

  let pos = ref 0

  let initial = (min_int, 0)
  let accumulator sl (e,p) =
    incr pos;
    if SumList.(sl.total) > e then sl.total, !pos else e,p
end)

(* Boolean indicates if we reached end of file *)
let rec read_one_elf acc =
  try
    let line = read_line () in
    match int_of_string_opt line with
    | None -> acc, false
    | Some t -> read_one_elf (SumList.add_item acc t)
  with End_of_file -> acc, true

let rec read_all_elves acc =
  let elf, finished = read_one_elf SumList.empty in
  let acc = MaxList.add_item acc elf in
  if finished then acc else read_all_elves acc

(* let elves = read_all_elves MaxList.empty
let () =
  let t, p = elves.total in
  Format.printf "%d, %d\n" t p *)

(* ==== Part 2 : Max of Three ===== *)
module MaxThreeList = ListWithAcc(struct
  type elem = SumList.t
  type acc = int * int * int (* three top total, in ascending order *)

  let initial = (min_int, min_int, min_int)

  let accumulator sl (a,b,c) =
    let total = SumList.(sl.total) in
    if total > a then (total, a, b)
    else if total > b then (a, total, b)
    else if total > c then (a, b, total)
    else (a, b, c)
end)

let rec read_all_elves2 acc =
  let elf, finished = read_one_elf SumList.empty in
  let acc = MaxThreeList.add_item acc elf in
  if finished then acc else read_all_elves2 acc

let elves = read_all_elves2 MaxThreeList.empty
let () =
  let t, p, q = elves.total in
  Format.printf "%d, %d, %d = %d\n" t p q (t + p + q)
