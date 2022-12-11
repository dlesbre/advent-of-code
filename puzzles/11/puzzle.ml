(* ==== Puzzle 11 : https://adventofcode.com/2022/day/11 ==== *)

(** Returns the suffix of str starting at start *)
let string_suffix str start =
  String.sub str start (String.length str - start)

(** Returns a list of all lines in the input, first line as hd *)
let rec read_all_lines acc =
  try
    let line = read_line () in
    read_all_lines (line::acc)
  with End_of_file ->
    List.rev acc

(** Our numbers are too large, so we only save their modulo
    (since all tests are divisibility test).
    Here is a modulo for operation on integers modulo a list of interesting dividends *)
module IntModulo(A : sig
  val modulos : int list
end) : sig
  type t

  val zero : t
  val one : t
  val of_int : int -> t

  val add : t -> t -> t
  val mul : t -> t -> t
  val modulo : t -> int -> int (* modulo only allowed on values in A.modulos *)
end  = struct
  type t = int list

  let zero = List.init (List.length A.modulos) (fun _ -> 0)
  let one = List.init (List.length A.modulos) (fun _ -> 1)

  let of_int x = List.map (fun a -> x mod a) A.modulos

  let rec map3 f a b c = match a, b, c with
    | [], [], [] -> []
    | a::a_s, b::bs, c::cs -> (f a b c)::(map3 f a_s bs cs)
    | _ -> raise (Invalid_argument "lists of different lengths)")

  let add a b = map3 (fun a b c -> (a + b) mod c) a b A.modulos
  let mul a b = map3 (fun a b c -> (a * b) mod c) a b A.modulos

  let modulo obj modu = (* m must be in A.modulos *)
    let rec runner a m = match a, m with
      | a::as', m::ms ->
          if m = modu then a else runner as' ms
      | _, _ -> failwith "not found"
    in runner obj A.modulos
end

(* Since we only know the full list of modulo at the end
   we start by parsing with int, and then convert to IntMod.t
   so we make our types parametric in 'a
   which will always be some variant of integer)*)
type 'a operation =
  | Add of 'a
  | Mul of 'a
  | Square

type 'a monkey = {
  mutable items : 'a list;
  operation : 'a operation;
  divisibility_test : int;
  on_true : int;
  on_false : int;
  mutable items_inspected : int;
}

let parse_items items =
  let items = String.split_on_char ',' items in
  List.map (fun x -> int_of_string (String.trim x)) items

let parse_operation operations =
  match operations.[0] with
  | '+' -> Add (int_of_string (string_suffix operations 2))
  | '*' ->
      if operations = "* old" then Square else
      Mul (int_of_string (string_suffix operations 2))
  | _ -> failwith ("Unknown operation "^operations)

let parse_monkey lines =
  match lines with
  | mk::starting::operation::test::if_true::if_false::_::r (* ignore blank lines *)
  | mk::starting::operation::test::if_true::if_false::r ->
      assert(String.starts_with ~prefix:"Monkey " mk);
      {
        items = parse_items (string_suffix starting 17);
        operation = parse_operation (string_suffix operation 23);
        divisibility_test = int_of_string (string_suffix test 21);
        on_true = int_of_string (string_suffix if_true 29);
        on_false = int_of_string (string_suffix if_false 30);
        items_inspected = 0;
      }, r
  | _ -> failwith "Too few lines"

let parse () =
  let rec runner = function
    | [] -> []
    | lines -> let monkey, rest = parse_monkey lines in
               monkey::(runner rest)
  in
  runner (read_all_lines [])

let monkeys = parse ()

module IntMod = IntModulo(struct
  let modulos = List.map (fun x -> x.divisibility_test) monkeys
end)

let to_modulo monkey =
  let operation = match monkey.operation with
  | Add x -> Add (IntMod.of_int x)
  | Mul x -> Mul (IntMod.of_int x)
  | Square -> Square in
  { monkey with items = List.map IntMod.of_int monkey.items; operation }

let monkeys = Array.of_list (List.map to_modulo monkeys)

let perform_operation old = function
  | Add x -> IntMod.add old x
  | Mul x -> IntMod.mul old x
  | Square -> IntMod.mul old old

let inspect_item monkey item = (* item assumed already popped from list *)
  (* if max_int / item < 3 then failwith "overflow"; *)
  let worry = perform_operation item monkey.operation in
  (* let worry = worry / 3 in *) (* uncomment for part1 *)
  if IntMod.modulo worry monkey.divisibility_test = 0 then
    monkeys.(monkey.on_true).items <- monkeys.(monkey.on_true).items @ [worry]
  else
    monkeys.(monkey.on_false).items <- monkeys.(monkey.on_false).items @ [worry];
  monkey.items_inspected <- monkey.items_inspected + 1

let rec inspect_all_items monkey =
  match monkey.items with
  | [] -> ()
  | t::q ->
      monkey.items <- q;
      inspect_item monkey t;
      inspect_all_items monkey

let perform_round () = Array.iter inspect_all_items monkeys

let rec do_n_rounds = function
  | 0 -> ()
  | n -> perform_round (); do_n_rounds (n-1)

let max2 (a,b) c = (* assumes a >= b *)
  if c > a then (c,a) else
  if c > b then (a,c) else
  (a,b)

let monkey_business ()=
  let a, b = Array.fold_left (fun m mk -> max2 m mk.items_inspected) (min_int, min_int) monkeys in
  a * b

let main () =
  (* do_n_rounds 20; *)
  do_n_rounds 10000;
  Format.printf "%d\n" (monkey_business ())

let () = main ()
