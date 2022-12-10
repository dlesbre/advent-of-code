(* ==== Puzzle 10 : https://adventofcode.com/2022/day/10 ==== *)

(** Returns the suffix of str starting at start *)
let string_suffix str start =
  String.sub str start (String.length str - start)




type instr =
  | Noop
  | Addx of int

let nb_cycle = function
  | Noop -> 1
  | Addx _ -> 2

let parse_line line =
  if line = "noop" then Noop
  else if String.starts_with ~prefix:"addx " line then
    Addx (int_of_string (string_suffix line 5))
  else failwith "Unknown instruction"

(** Returns a list of all lines in the input, first line as hd *)
let rec read_all_lines acc =
  try
    let line = read_line () in
    let contents = parse_line line in
    read_all_lines (contents::acc)
  with End_of_file ->
    List.rev acc

type state = {
  x : int;
  nb_cycle: int;
}

let exec_instr state op =
  let new_state = match op with
  | Noop -> state
  | Addx x -> { state with x=state.x + x } in
  { new_state with nb_cycle = new_state.nb_cycle + nb_cycle op }

let rec signal_strength instr next state score =
  match next with
  | [] -> score
  | t::q ->
      (* Format.printf "%d: x=%d, n=%d, s=%d\n" t state.x state.nb_cycle score; *)
      match instr with
      | [] -> failwith "Program too short"
      | op::ops ->
          let state' = exec_instr state op in
          if state'.nb_cycle >= t then
            signal_strength ops q state' (t * state.x + score)
          else
            signal_strength ops (t::q) state' score

let part1 () =
  let lines = read_all_lines [] in
  let strength = signal_strength lines [20;60;100;140;180;220] { x=1; nb_cycle=0 } 0 in
  Format.printf "%d\n" strength

let crt = Array.init 6 (fun _ -> Array.make 40 false)

let make_crt state crt =
  let nb = state.nb_cycle - 1 in
  let line = nb / 40 in
  if line < 6 then
    let col = nb mod 40 in
    let lit = abs (col - state.x) <= 1 in
    Format.printf "(%d,%d) x=%d n=%d lit=%b\n" line col state.x state.nb_cycle lit;
    crt.(line).(col) <- lit

let draw_crt crt =
  Array.iter (
    fun line ->
      Array.iter (fun b -> Format.printf "%c" (if b then '#' else '.')) line;
      Format.printf "\n"
  ) crt

let rec calc instr state =
    (* Format.printf "%d: x=%d, n=%d, s=%d\n" t state.x state.nb_cycle score; *)
    match instr with
    | [] -> crt
    | op::ops ->
        make_crt state crt;
        let state' = exec_instr state op in
        if op <> Noop
        then make_crt {state with nb_cycle = state.nb_cycle+1} crt;
        calc ops state'

let part2 () =
  let lines = read_all_lines [] in
  let crt = calc lines { x=1; nb_cycle=1 } in
  draw_crt crt

let () = part2 ()
