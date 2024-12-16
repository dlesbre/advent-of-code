(* ==== Puzzle 13 : https://adventofcode.com/2024/day/13 ====  *)



type machine = {
  button_A : Vec2.t;
  button_B : Vec2.t;
  prize: Vec2.t
}

let rec parse machines = function
  | [] -> List.rev machines
  | ""::rest -> parse machines rest
  | a::b::c::rest ->
      let button_A = Scanf.sscanf a "Button A: X+%d, Y+%d" (fun x y -> x,y) in
      let button_B = Scanf.sscanf b "Button B: X+%d, Y+%d" (fun x y -> x,y) in
      let prize = Scanf.sscanf c "Prize: X=%d, Y=%d" (fun x y -> x,y) in
      parse ({button_A; button_B; prize}::machines) rest
  | _ -> failwith "Invalid format"
let preprocess = parse []

(** Solve the system: {v
    [ prize.X ] = [ A.X  B.X ] [ unknown.X ]
    [ prize.Y ]   [ A.Y  B.Y ] [ unknown.Y ]
v} *)
let solve_machine m =
  let det = Vec2.det m.button_A m.button_B in
  if det = 0 then None
  else
    let a, b = Vec2.inverse m.button_A m.button_B in
    let push_A = (fst a)*(fst m.prize) + (fst b)*(snd m.prize) in
    let push_B = (snd a)*(fst m.prize) + (snd b)*(snd m.prize) in
    if push_A mod det = 0 && push_B mod det = 0
    then Some (push_A / det, push_B / det)
    else None

let count_token (a,b) = 3*a+b

let cst = 10000000000000

let part1 input =
  let l = List.filter_map solve_machine input in
  let l = List.filter (fun (x, y) -> x <= 100 && y <= 100) l in
  List.fold_left (fun total nb -> total + count_token nb) 0 l

let part2 input =
  let input_corrected = List.map (fun x -> { x with prize = Vec2.(x.prize +| (cst, cst))}) input in
  let l = List.filter_map solve_machine input_corrected in
  List.fold_left (fun total nb -> total + count_token nb) 0 l

  let () = register_int ~year:2024 ~day:13 ~preprocess ~part1 ~part2
