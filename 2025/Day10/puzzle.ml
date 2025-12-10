(* ==== Puzzle 10 : https://adventofcode.com/2025/day/10 ====  *)

module Lights = Bitset.Make(struct
  type t = int
  let singleton x = assert (0 <= x && x < Sys.int_size); 1 lsl x
end)

type machine = {
  indicator_light: Lights.t;
  buttons: int list list;
  joltage: int list;
}

let parse_indicator_light str =
  string_foldi (fun i c lights -> match c with
    | '#' -> Lights.union (Lights.singleton i) lights
    | '.' -> lights
    | _ -> failwith "Invalid light indicator")
  str Lights.empty

let parse_int_list str = String.split_on_char ',' str |> List.map int_of_string

let parse_line str =
  match String.split_on_char ' ' str |> List.map (string_slice ~start:1 ~stop:(-1)) with
  | [] -> failwith "empty line"
  | lights::line ->
      let indicator_light = parse_indicator_light lights in
      match List.rev line with
        | [] -> failwith "line too short"
      | joltage::buttons ->
          let joltage = parse_int_list joltage in
          let buttons = List.map parse_int_list buttons in
          { indicator_light; buttons; joltage }

let preprocess = List.map parse_line

let option_min a b = match a,b with
  | None, None -> a
  | Some _, None -> a
  | None, Some _ -> b
  | Some a, Some b -> Some (min a b)

(** XOR is associative and commutative. So the simplest option
    presses each button at most once *)
let rec button_presses_p1 n lights = function
  | [] -> None
  | b::bs -> let pressed = Lights.disjoint_union b lights in
            if pressed = Lights.empty then Some (n+1) else
            match button_presses_p1 n lights bs with
            | Some i when i <= n+2 -> Some i (* we know this must be minimal *)
            | opt -> button_presses_p1 (n+1) pressed bs |> option_min opt

let button_presses_p1 { indicator_light; buttons; _ } =
  let buttons = List.map Lights.of_list buttons in
  (* Going from no light to the given set is the same as doing the reverse *)
  match button_presses_p1 0 indicator_light buttons with
  | Some n -> n
  | None -> failwith "Could not get correct configuration"

let part1 = list_sum button_presses_p1

let z3_solve { buttons; joltage; _ } =
  let context = Z3.mk_context [] in
  let optim = Z3.Optimize.mk_opt context in
  let buttons_v = List.mapi (fun i b -> Z3.Arithmetic.Integer.mk_const context @@ Z3.Symbol.mk_int context i, b) buttons in
  let joltage_constraints = List.mapi (fun j joltage ->
    let vars = List.filter_map (fun (var,button) -> if List.mem j button then Some var else None) buttons_v in
    let expr = Z3.Arithmetic.mk_add context vars in
    let bound = Z3.Arithmetic.Integer.mk_numeral_i context joltage in
    [Z3.Arithmetic.mk_le context expr bound; Z3.Arithmetic.mk_le context bound expr]
    ) joltage in
  let zero = Z3.Arithmetic.Integer.mk_numeral_i context 0 in
  let pos_constraints = List.map (fun (v,_) -> Z3.Arithmetic.mk_le context zero v) buttons_v in
  List.iter (Z3.Optimize.add optim) joltage_constraints;
  Z3.Optimize.add optim pos_constraints;
  let target = List.map fst buttons_v |> Z3.Arithmetic.mk_add context in
  let handle = Z3.Optimize.minimize optim target in
  match Z3.Optimize.check optim with
  | UNSATISFIABLE
  | UNKNOWN -> failwith "no solution found"
  | SATISFIABLE ->
      Z3.Optimize.get_lower handle
      |> Z3.Arithmetic.Integer.get_big_int
      |> Z.to_int

let part2 = list_sum z3_solve

let () = register_int ~year:2025 ~day:10 ~preprocess ~part1 ~part2
