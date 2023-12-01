(* ==== Puzzle 20 : https://adventofcode.com/2022/day/20 ==== *)

module CircularList: sig
  type 'a t
  val init : 'a list -> 'a t
  val get : 'a t -> int -> 'a
  val pos : 'a t -> int
  val shift : 'a t -> int -> 'a t
  val find_first : 'a t -> ('a -> bool) -> 'a t option
end = struct
  type 'a t = {
    prev : 'a list;
    next : 'a list;
    size : int;
    pos : int;
  }

  let init x = { prev=[]; next=x; size=List.length x; pos=0 }

  let canonical_coordinate t x =
    if x >= 0
    then x mod t.size
    else
      let x = ((-x) mod t.size) in
      if x = 0 then 0 else t.size - x

  let pos x = x.pos

  let to_head t = { t with prev=[]; pos=0; next=List.rev_append t.prev t.next }

  let move_forward t = {
    t with
    pos=t.pos+1;
    next=List.tl t.next;
    prev=List.hd t.next :: t.prev
  }

  let move_back t = {
    t with
    pos=t.pos-1;
    prev=List.tl t.prev;
    next=List.hd t.prev :: t.next
  }

  let rec move_to x t = (* assumes x is canonical position *)
    match x - t.pos with
    | 0 -> t
    | i when i < 0 -> move_to x (move_back t)
    | _ -> move_to x (move_forward t)

  let get_hd t = List.hd t.next
  let set_hd t v = { t with next = v :: List.tl t.next }

  let pop t = (* remove element at current position *)
    get_hd t, { t with size=t.size-1; next=List.tl t.next }

  let add t v = (* add element at current position *)
    { t with size=t.size+1; next=v::t.next }

  let shift t i = (* shift element at current position by i *)
    let v, t = pop t in
    let x = canonical_coordinate t (t.pos + i) in
    let t = move_to x t in
    add t v

  let find_first t f =
    let rec aux t =
      if f (get_hd t)
      then Some t
      else
        if t.pos + 1 = t.size
        then None
        else aux (move_forward t)
    in aux (to_head t)

  let get t i =
    let t = move_to (canonical_coordinate t i) t in
    get_hd t
end

(** Returns a list of all lines in the input, first line as hd *)
let rec read_all_lines acc =
  try
    let line = read_line () in
    let contents = int_of_string line in
    read_all_lines (contents::acc)
  with End_of_file ->
    List.rev acc

let mix list numbers = List.fold_left (fun list (i,n) ->
    match CircularList.find_first list (fun (j,_) -> j = i) with
    | None -> failwith "No i-th element"
    | Some t -> CircularList.shift t n
    ) list numbers

let part1 () =
  let lines = read_all_lines [] in
  let numbers = List.mapi (fun i n -> i, n) lines in
  let list = CircularList.init numbers in
  let list = mix list numbers in
  let list = Option.get (CircularList.find_first list (fun (_,x) -> x = 0)) in
  let p = CircularList.pos list in
  let (_, n1000) = CircularList.get list (p+1000) in
  let (_, n2000) = CircularList.get list (p+2000) in
  let (_, n3000) = CircularList.get list (p+3000) in
  Format.printf "%d, %d, %d -> %d@." n1000 n2000 n3000 (n1000 + n2000 + n3000)

(* let () = part1 () *)

let rec multimix n list numbers =
  if n = 0 then list else multimix (n-1) (mix list numbers) numbers

let part2 () =
  let lines = read_all_lines [] in
  let decrypt_key = 811589153 in
  let numbers = List.mapi (fun i n -> i, n*decrypt_key) lines in
  let list = CircularList.init numbers in
  let list = multimix 10 list numbers in
  let list = Option.get (CircularList.find_first list (fun (_,x) -> x = 0)) in
  let p = CircularList.pos list in
  let (_, n1000) = CircularList.get list (p+1000) in
  let (_, n2000) = CircularList.get list (p+2000) in
  let (_, n3000) = CircularList.get list (p+3000) in
  Format.printf "%d, %d, %d -> %d@." n1000 n2000 n3000 (n1000 + n2000 + n3000)

let () = part2 ()
