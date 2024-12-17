(* ==== Puzzle 17 : https://adventofcode.com/2024/day/17 ====  *)



type register = A | B | C
type instruction = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv
type 'a memory = {
  register_A: 'a;
  register_B: 'a;
  register_C: 'a;
  pc: int;
  out: int list;
}
let get_reg memory = function A -> memory.register_A | B -> memory.register_B | C -> memory.register_C
let set_reg memory register value = match register with
  | A -> { memory with register_A = value; pc = memory.pc + 2 }
  | B -> { memory with register_B = value; pc = memory.pc + 2 }
  | C -> { memory with register_C = value; pc = memory.pc + 2 }

let combo_operand memory = function
  | n when n < 4 -> n
  | 4 -> memory.register_A
  | 5 -> memory.register_B
  | 6 -> memory.register_C
  | _ -> failwith "Invalid combo operand"

let instruction = function
  | 0 -> Adv
  | 1 -> Bxl
  | 2 -> Bst
  | 3 -> Jnz
  | 4 -> Bxc
  | 5 -> Out
  | 6 -> Bdv
  | 7 -> Cdv
  | _ -> failwith "Invalid instruction"

(*
2,4, B = A & 0b111 (Bst 4)
1,5, B = B xor 0b101 (Bxl 5) = A & 0b111 xor 0b101
7,5, C = A >> B      (Cdv 5) =
4,3, B = B xor C
1,6, B = B xor 0b110 (Bxl 6)
0,3, A = A >> 3      (Adv 3)
5,5, out B
3,0  goto 0 if A non-zero



*)

let step operation operand memory = match operation with
  | Adv -> set_reg memory A (memory.register_A lsr (combo_operand memory operand))
  | Bxl -> set_reg memory B (memory.register_B lxor operand)
  | Bst -> set_reg memory B (combo_operand memory operand land 0b111)
  | Jnz -> if memory.register_A = 0 then {memory with pc=memory.pc+2} else {memory with pc=operand}
  | Bxc -> set_reg memory B (memory.register_B lxor memory.register_C)
  | Out -> { memory with out = (combo_operand memory operand land 0b111)::memory.out; pc=memory.pc+2 }
  | Bdv -> set_reg memory B (memory.register_A lsr (combo_operand memory operand))
  | Cdv -> set_reg memory C (memory.register_A lsr (combo_operand memory operand))

let preprocess = function
  | a::b::c::""::e::[] -> {
      register_A = Scanf.sscanf a "Register A: %d" Fun.id;
      register_B = Scanf.sscanf b "Register B: %d" Fun.id;
      register_C = Scanf.sscanf c "Register C: %d" Fun.id;
      out = [];
      pc=0 },
      String.sub e 9 (String.length e - 9) |>
      String.split_on_char ',' |>
      List.map int_of_string |>
      Array.of_list
  | _ -> failwith "Invalid format"

let rec part1 (memory, array) =
  match array_get_opt array (memory.pc+1) with
  | None -> List.rev memory.out |> List.map string_of_int |> String.concat ","
  | Some operand ->
    (* Format.printf "A=%d; B=%d; C=%d; op=%d; operand=%d" memory.register_A memory.register_B memory.register_C  *)
    part1 (step (instruction array.(memory.pc)) operand memory, array)

module IntSet = Set.Make(Int)

type bit =
  | Unknown of int (** Unknown bit at position i in the initial value of a *)
  | Xor of IntSet.t * bool (** set of unknowns that appear in xor, and bool if flipped *)
  | Zero
  | One

type truth_value = False | True | Either

module SymbolicInt = struct

  type t = int list
  let size = 32
  let unknown = List.init size (fun x -> Unknown x)
  let of_int size n = List.init size (fun x -> if (n lsr x) land 1 = 1 then One else Zero)
  let rec ( lsr ) a n =
    if n = 0 then a else
    match a with [] -> [] | _::a -> a lsr (n-1)

  let xor set bool =
    if IntSet.is_empty set then if bool then One else Zero
    else if not bool && IntSet.cardinal set = 1 then Unknown (IntSet.choose set)
    else Xor(set, bool)

  let bxor a b = if a then not b else b

  let[@tail_mod_cons] rec ( lxor ) a b = match a, b with
    | [], c | c, [] -> c
    | a::a', b::b' ->
        let c = match a, b with
        | Zero, c | c, Zero -> c
        | One, One -> Zero
        | One, Xor(c, b) | Xor(c, b), One -> xor c (not b)
        | One, Unknown x | Unknown x, One -> Xor(IntSet.singleton x, true)
        | Unknown a, Unknown b -> if a = b then Zero else Xor (IntSet.singleton a |> IntSet.add b, false)
        | Xor(a,b), Xor(c,d) -> xor (IntSet.diff (IntSet.union a c) (IntSet.inter a c)) (bxor b d)
        | Xor(a,b), Unknown c
        | Unknown c, Xor(a,b) -> xor (IntSet.add c a) b
        in c :: (a' lxor b')

  let force_truth = function
    | False -> Either
    | x -> x
  let rec truth_value = function
    | [] -> False
    | One::_ -> True
    | Zero::rest -> truth_value rest
    | Xor(_,_)::rest
    | Unknown _::rest ->force_truth (truth_value rest)

  let rec mask n x = match n,x with
    | 0, _ | _, [] -> []
    | n, x::xs -> x::mask (n-1) xs
end

let combo_operand memory = function
  | n when n < 4 -> SymbolicInt.of_int 3 n
  | 4 -> memory.register_A
  | 5 -> memory.register_B
  | 6 -> memory.register_C
  | _ -> failwith "Invalid combo operand"

(* let step2 operation operand memory =
  let open SymbolicInt in
  match operation with
  | Adv -> set_reg memory A (memory.register_A lsr (combo_operand memory operand))
  | Bxl -> set_reg memory B (memory.register_B lxor of_int 3 operand)
  | Bst -> set_reg memory B (mask 3 @@ combo_operand memory operand)
  | Jnz -> if memory.register_A = 0 then {memory with pc=memory.pc+2} else {memory with pc=operand}
  | Bxc -> set_reg memory B (memory.register_B lxor memory.register_C)
  | Out -> { memory with out = (combo_operand memory operand land 0b111)::memory.out; pc=memory.pc+2 }
  | Bdv -> set_reg memory B (memory.register_A lsr (combo_operand memory operand))
  | Cdv -> set_reg memory C (memory.register_A lsr (combo_operand memory operand)) *)

let part2 (memory,array) =
  for i = 0 to Array.length array / 2 do

  done
  "0x"

let () = register ~year:2024 ~day:17 ~preprocess ~part1 ~part2
