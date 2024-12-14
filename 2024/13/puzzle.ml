(* ==== Puzzle 13 : https://adventofcode.com/2024/day/13 ====  *)

module Vec2 : sig
  type t = int * int

  val compare: t -> t -> int

  val ( +| ) : t -> t -> t
  val ( -| ) : t -> t -> t
  val ( ~| ) : t -> t (** unary minus *)
  val ( *| ) : int -> t -> t

  val norm_manhattan : t -> int
  val dist_manhattan : t -> t -> int

  val dot : t -> t -> int

  val det: t -> t -> int
  (** Determinant of the 2-2 matrix made of both vectors *)

  val inverse: t -> t -> t * t
  (** Inverse of the matrix (not multiplied by 1/det!) *)

  val pp : Format.formatter -> t -> unit
end = struct
  type t = int * int

  let compare (x,y) (x',y') =
    let cmp = Int.compare x x' in
    if cmp = 0 then Int.compare y y' else cmp

  let ( +| ) (x,y) (x',y') = x+x', y+y'
  let ( -| ) (x,y) (x',y') = x-x', y-y'
  let ( ~| ) (x,y) = (-x,-y)
  let ( *| ) n (x,y) = n*x, n*y

  let norm_manhattan (x,y) = abs x + abs y
  let dist_manhattan a b = a -| b |> norm_manhattan

  let pp fmt (x,y) = Format.fprintf fmt "(%d,%d)" x y

  let dot (x,y) (x',y') = x*x' + y*y'

  let det (x,y) (x',y') = x*y' - y*x'

  let inverse (x,y) (x',y') = (y',-y), (-x',x)
end


let rec read_all_lines acc =
  try read_all_lines (read_line ()::acc)
  with End_of_file -> List.rev acc

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

let main () =
  let input = parse [] (read_all_lines []) in
  let l = List.filter_map solve_machine input in
  (* let l = List.filter (fun (x, y) -> x <= 100 && y <= 100) l in *)
  let p1 = List.fold_left (fun total nb -> total + count_token nb) 0 l in
  Format.printf "Part 1 : %d@." p1;
  let input_corrected = List.map (fun x -> { x with prize = Vec2.(x.prize +| (cst, cst))}) input in
  let l = List.filter_map solve_machine input_corrected in
  let p2 = List.fold_left (fun total nb -> total + count_token nb) 0 l in
  Format.printf "Part 1 : %d@." p2

let () = main ()
