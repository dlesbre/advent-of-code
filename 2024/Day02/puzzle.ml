(* Puzzle 02 : https://adventofcode.com/2024/day/2 *)

let preprocess input = List.map (fun s -> List.map int_of_string (String.split_on_char ' ' s)) input


type folder =
  | Uninitialized
  | Initial of int
  | Increasing of int * bool (** bool is true if we have not yet removed an element *)
  | Decreasing of int * bool

exception Unsafe

let valid diff = 1 <= diff && diff <= 3

(** can_fail is true when we can remove *)
let check_safe list can_fail =
  match List.fold_left (fun acc elt -> match acc with
      | Uninitialized -> Initial elt
      | Initial x ->
          if valid (x - elt) then Decreasing (elt, can_fail)
          else if valid (elt - x) then Increasing (elt, can_fail)
          else raise Unsafe
      | Increasing (x, can_fail) ->
          if valid (elt - x)
          then Increasing (elt, can_fail)
          else
            if can_fail
            then Increasing (x, false)
            else raise Unsafe
      | Decreasing (x, can_fail) ->
          if valid (x - elt)
          then Decreasing (elt, can_fail)
          else
            if can_fail
            then Decreasing (x, false)
            else raise Unsafe
    ) Uninitialized list with
  | Uninitialized | Initial _ -> failwith "list too short..."
  | _ -> true
  | exception Unsafe -> false

let part1 reports =
  List.fold_left (fun acc v -> if check_safe v false then acc+1 else acc) 0 reports

let part2 reports =
  List.fold_left (fun acc v ->
    match v with
    | x::(_::z as tl) ->
      (* Since the first elements decide how we interpret the rest of the list,
         we treat them separatly: i.e. check the list without them explicitly.
         This is suboptimal, but lists are small enough that it doesn't matter. *)
      if check_safe v true || check_safe tl false || check_safe (x::z) false then acc+1 else acc
    | _ -> failwith "report too short"
) 0 reports


let () = register_int ~year:2024 ~day:02 ~preprocess ~part1 ~part2
