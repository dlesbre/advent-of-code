(* ==== Puzzle 21 : https://adventofcode.com/2022/day/21 ==== *)

type binop = Plus | Minus | Mult | Div

type int_tree =
  | Lit of int
  | Unknown
  | Binop of binop * int_tree * int_tree

type number =
  | N_Int of int_tree
  | N_Binop of string * binop * string

module StringMap = Map.Make(String)

(** Returns the suffix of str starting at start *)
let string_suffix str start =
  String.sub str start (String.length str - start)

let parse_binop = function
  | '+' -> Plus
  | '-' -> Minus
  | '*' -> Mult
  | '/' -> Div
  | _ -> failwith "Unknown binop"

let to_binop = function
  | Plus -> ( + )
  | Minus -> ( - )
  | Mult -> ( * )
  | Div -> ( / )

let parse_value v =
  match int_of_string_opt v with
  | Some x -> N_Int (Lit x)
  | None ->
      Scanf.sscanf v "%s %c %s" (fun l b r -> N_Binop(l, parse_binop b, r))


let parse_line line =
  let monkey = String.sub line 0 4 in
  let value = parse_value (string_suffix line 6) in
  monkey, value

(** Returns a list of all lines in the input, first line as hd *)
let rec read_all_lines acc =
  try
    let line = read_line () in
    let contents = parse_line line in
    read_all_lines (contents::acc)
  with End_of_file ->
    List.rev acc

let perform_binop b l r =
  match l, r with
  |Lit l, Lit r -> Lit (to_binop b l r)
  |_ -> Binop(b, l, r)

let rec calc part_is_2 monkey map =
  if part_is_2 && monkey = "humn" then Unknown, map else
  match StringMap.find monkey map with
  | N_Int i -> i, map
  | N_Binop(l,b,r) ->
      let l, map = calc part_is_2 l map in
      let r, map = calc part_is_2 r map in
      let s = perform_binop b l r in
      s, StringMap.add monkey (N_Int s) map

(* Thankfully, the unknown only appears once,
   No multiplying it with itself and having to solve polynomial equations *)
let rec solve value = function
  | Unknown -> value
  | Lit n -> failwith "Should not lead to literal"
  | Binop(Plus, Lit a, x)
  | Binop(Plus, x, Lit a) -> solve (value - a) x
  | Binop(Mult, Lit a, x)
  | Binop(Mult, x, Lit a) -> solve (value / a) x
  | Binop(Minus, Lit a, x) -> solve (a - value) x
  | Binop(Minus, x, Lit a) -> solve (value + a) x
  | Binop(Div, x, Lit a) -> solve (value * a) x
  | Binop(Div, Lit a, x) -> solve (a / value) x
  | _ -> failwith "Invalid binop"

let part1 () =
  let line = read_all_lines [] in
  let values = List.fold_left (fun map (monkey, value) ->
      StringMap.add monkey value map
    ) StringMap.empty line in
  let res, values = calc false "root" values in
  match res with
  | Lit res -> Format.printf "%d\n" res
  | _ -> failwith "Res is not a number :("

let part2 () =
  let line = read_all_lines [] in
  let values = List.fold_left (fun map (monkey, value) ->
      StringMap.add monkey value map
    ) StringMap.empty line in
  match StringMap.find "root" values with
  | N_Binop(l,b,r) ->
      let l, values = calc true l values in
      let r, values = calc true r values in
      let res = match l, r with
        | Lit l, u
        | u, Lit l-> solve l u
        | _,_ -> failwith "O god this is complex"
      in Format.printf "%d\n" res
  | _ -> failwith "Root should be a binop"

let () = part2 ()
