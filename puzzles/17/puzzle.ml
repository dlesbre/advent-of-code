(* ==== Puzzle 17 : https://adventofcode.com/2022/day/17 ==== *)

type direction = Left | Right

(** Fast operation on set of small integers *)
module BitSet : sig
  (* Requires values >= 0 and <= 63 *)
  type t
  val singleton : int -> t
  val empty : t
  val empty_line : t
  val mem : t -> int -> bool
  val add : t -> int -> t
  val is_empty : t -> bool
  val inter : t -> t -> t
  val union : t -> t -> t
  val of_list : int list -> t
  val shift : direction -> t -> t

  type pack
  val pack : t list -> pack (* concat top 18 elements intos a single packed value *)
  val unpack : pack -> t list
end = struct
  type t = int
  type pack = int * int
  let empty = 0
  let singleton x = 1 lsl x
  let mem t x = t land (singleton x) <> 0
  let add t x = t lor (singleton x)

  let is_empty x = x = 0
  let inter a b = a land b
  let union a b = a lor b

  let of_list l = List.fold_left add empty l

  let empty_line = of_list [0; 8]

  let shift dir x =
    match dir with (* our lines have 0 on the left and 9 on the right *)
    | Left -> x lsr 1
    | Right -> x lsl 1

  let mask = 0b1111111

  let pack l =
    let rec aux n m l =
    match m, l with
      | 0, q -> n, q
      | m, x::q ->
          (* strip top/bottom bits *)
          let x = (x lsr 1) land mask in
          aux ((n lsl 7) lor x) (m-1) q
      | _, [] -> n, []
    in
    let top, l = aux 0 9 l in (* 9 * 7 bits = 63 bits *)
    let bot, _ = aux 0 9 l in
    top, bot

  let unpack (top, bot) =
    let rec aux n m i =
      match m with
      | 0 -> n
      | _ -> let x = ((i land mask) lsl 1) lor empty_line in
             aux (x::n) (m-1) (i lsr 7)
    in aux [] 9 top @ aux [] 9 bot
end

module Cycle : sig
  type 'a t

  val init : 'a list -> 'a t
  val next : 'a t -> 'a t
  val get : 'a t -> 'a
  val pos : 'a t -> int
end = struct
  type 'a t = {
    current : 'a list;
    start : 'a list;
    pos : int;
  }
  let init x = { current=x; start=x; pos=0 }
  let get x = List.hd x.current
  let next x = match x.current with
    | [] | _::[] -> { x with current = x.start; pos=0 }
    | t::q -> { x with current = q; pos=x.pos+1 }

  let pos x = x.pos
end

type shape = H_Line | Plus | Corner | V_Line | Square

type line = BitSet.t
(* We make lines 9 wide with two virtual rocks on the sides *)
let empty_line = BitSet.empty_line
let full_line = BitSet.of_list [0;1;2;3;4;5;6;7;8]

let shape_h_line = [BitSet.of_list [3;4;5;6]]
let shape_plus = [
  BitSet.singleton 4;
  BitSet.of_list [3;4;5];
  BitSet.singleton 4;
]
let shape_corner = [
  BitSet.singleton 5;
  BitSet.singleton 5;
  BitSet.of_list [3;4;5];
]
let shape_v_line = [
  BitSet.singleton 3;
  BitSet.singleton 3;
  BitSet.singleton 3;
  BitSet.singleton 3;
]

let shape_square = [
  BitSet.of_list [3;4];
  BitSet.of_list [3;4];
]

let shape = function
  | H_Line -> shape_h_line
  | Plus ->shape_plus
  | Corner -> shape_corner
  | V_Line -> shape_v_line
  | Square -> shape_square

let shapes = Cycle.init [H_Line; Plus; Corner; V_Line; Square]

let print_line l =
  Format.printf "|";
  for k = 1 to 7 do
    Format.printf "%c" (if BitSet.mem l k then '#' else '.')
  done;
  Format.printf "|@."

let print_pit pit =List.iter print_line pit; Format.printf "@."

let rec add_empty n x =
  match n with
  | 0 -> x
  | n -> add_empty (n-1) (empty_line::x)

let rec can_be shape pit =
  match shape, pit with
  | [], _ -> true
  | s::ss, l::ls -> BitSet.is_empty (BitSet.inter s l) && can_be ss ls
  | _, [] -> failwith "Shape clips out of pit"

let shift dir = List.map (BitSet.shift dir)

let rec place_shape shape pit =
  match shape, pit with
  | [], _ -> pit
  | s::ss, p::ps -> BitSet.union s p :: place_shape ss ps
  | _, [] -> failwith "Shape falls out of pit"

(** Move a shape until it settles *)
let rec move wind shape ht pit =
  let shifted = shift (Cycle.get wind) shape in
  let shape = if can_be shifted pit then shifted else shape in
  let wind = Cycle.next wind in
  let pit' = List.tl pit in
  if can_be shape pit'
  then
    let line = List.hd pit in
    if line = empty_line
    then move wind shape (ht+1) pit'
    else let w, m, ht = move wind shape (ht+1) pit' in w, line::m, ht
  else wind, place_shape shape pit, ht

module MemMap = Map.Make(struct
  type t = int * BitSet.pack (* wind_index * top_rows *)
  let compare = compare
end)

exception Found_Loop of MemMap.key * direction Cycle.t * int * int * int * int

let rec matches_start a b =
  match a, b with
  | [], _ -> true
  | a::as', b::bs' -> a = b && matches_start as' bs'
  | _, _ -> false

let rec do_n_drops n ht wind rocks pit map fail =
  match n with
  | 0 -> pit, ht
  | _ ->
      let map =
        if fail && Cycle.pos rocks = 0 && ht > 18 then
          let key = (Cycle.pos wind, BitSet.pack pit) in
          begin match MemMap.find_opt key map with
          | Some (n',ht') -> raise (Found_Loop(key, wind, n', ht', n, ht))
          | None -> MemMap.add key (n,ht) map
          end
        else map in
      let rock_shape = shape (Cycle.get rocks) in
      let add = 3 + List.length rock_shape in
      let pit = add_empty add pit in
      let wind, pit, down = move wind rock_shape 0 pit in
      do_n_drops (n-1) (max ht (ht + add - down)) wind (Cycle.next rocks) pit map fail

let do_n_drops wind nb =
  try
    do_n_drops nb 0 wind shapes [full_line] MemMap.empty true
  with
    Found_Loop((_, packed), wind, start_n, start_ht, end_n, end_ht) ->
      let loop_length = start_n - end_n in
      let loop_incr = end_ht - start_ht in
      let nb_loops = end_n / loop_length in
      let nb_remain = (end_n - nb_loops*loop_length) in
      let new_ht = end_ht + nb_loops*loop_incr in
      Format.printf "Loop : %d -> %d@.length = %d, height = %d, nb_needed = %d@.Remain %d %d@."
        start_n end_n
        loop_length loop_incr nb_loops
        nb_remain new_ht;
      let pit = BitSet.unpack packed in
      do_n_drops nb_remain new_ht wind shapes pit MemMap.empty false


let main () =
  let wind = read_line () in
  let wind = Cycle.init (String.fold_right (fun c l -> if c = '<' then Left::l else Right::l) wind []) in
  let pit, ht = do_n_drops wind 1_000_000_000_000 in
  Format.printf "%d %d@." (List.length pit - 1) ht

let () = main ()
