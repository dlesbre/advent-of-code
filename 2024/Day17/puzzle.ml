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
  | Some operand -> part1 (step (instruction array.(memory.pc)) operand memory, array)


(** Run program until first print, return that print *)
let rec step_to_out memory array =
  match memory.out with
  | [] -> step_to_out (step (instruction array.(memory.pc)) array.(memory.pc + 1) memory) array
  | x::[] -> x
  | _::_ -> failwith "Multiple outs in a single instruction"

(** This only works because the program is very simple:
    - it is a simple loop, starting at instruction 0
    - there is a single "out" per loop iteration
    - B and C are written before being read on each iteration
      (we can ignore their initial value)
    - A is right-shifted by 3-bits every iteration
    With these constraints, we can compute the value of A 3-bits at a time.
    We start by the last prints, find a value that matches, shift it left by 3
    and repeat.

    Some backtracking is needed because the printed value at each iteration
    depends on the last six bits of A, not just the last 3. *)
let rec find_A a memory array = function
  | [] -> Some a
  | x::xs ->
      let a = a lsl 3 in
      let rec aux = function
        | 8 -> None
        | n ->
          let register_A = a lor n in
          if step_to_out {memory with register_A } array = x
          then
          match find_A register_A memory array xs with
          | None -> aux (n+1)
          | some -> some
          else aux (n+1)
      in aux 0


let part2 (memory,array) =
  let printed_values = Array.to_list array |> List.rev in
  let register_A = Option.get @@ find_A 0 memory array printed_values in
  let output = part1 ({memory with register_A}, array) in
  Format.sprintf "A = %d; Program = %s; Output = %s"
    register_A
    (Array.to_list array |> List.map string_of_int |> String.concat ",")
    output

let () = register ~year:2024 ~day:17 ~preprocess ~part1 ~part2
