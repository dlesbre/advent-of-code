type pair = {
  left: int;
  right: int;
}

let included p1 p2 =
  p2.left <= p1.left && p1.right <= p2.right

let read_pairs line =
  Scanf.sscanf line "%d-%d,%d-%d" (fun a b c d ->
      {left=a; right=b}, {left=c; right=d}
    )

let rec read_input accumulator =
  try
    let line = read_line () in
    read_input (read_pairs line::accumulator)
  with End_of_file -> accumulator

let part1 () =
  let pairs = read_input [] in
  let contains = List.filter (fun (l,r) -> included l r || included r l) pairs in
  Format.printf "%d\n" (List.length contains)

  (* let () = part1 () *)

let contains pair int = pair.left <= int && int <= pair.right

let overlap p1 p2 = contains p1 p2.left || contains p1 p2.right

let part2 () =
  let pairs = read_input [] in
  let contains = List.filter (fun (l,r) -> overlap l r || overlap r l) pairs in
  Format.printf "%d\n" (List.length contains)

let () = part2 ()
