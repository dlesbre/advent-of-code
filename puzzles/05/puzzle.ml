let get_stack chars =
  match chars with
  | "   " -> None
  | _ -> Scanf.sscanf chars "[%c]" (fun n -> Some n)

let rec read_stacks line =
  let len = String.length line in
  if len = 0 then []
  else
    let next = if len > 4 then read_stacks (String.sub line 4 (len-4)) else [] in
    get_stack (String.sub line 0 3)::next

let rec make_stacks acc =
  let line = read_line () in
  if not (line.[0] = ' ' && line.[1] <> ' ') then
    let stacks = read_stacks line in
    if acc <> [] then assert(List.length stacks = List.length (List.hd acc));
    make_stacks (stacks::acc)
  else
    (ignore (read_line ()); acc) (* Read extra empty line *)

let rotate_stacks stacks =
  let n = List.length (List.hd stacks) in
  let arr = Array.make n [] in
  List.iter (List.iteri (fun i s -> match s with
                         | None -> ()
                         | Some x -> arr.(i) <- x::(arr.(i)))
  ) stacks; arr

let pp_l = Format.pp_print_list Format.pp_print_char
let pp_a fmt =
  Array.iteri (fun i l -> Format.fprintf fmt "%d: [%a]\n" (i+1) pp_l l)

let rec do_action stacks nb from to_ =
  let from' = from - 1 in
  let to' = to_ - 1 in
  match nb with
  | 0 -> ()
  | _ -> let move = List.hd stacks.(from') in
         stacks.(from') <- List.tl stacks.(from');
         stacks.(to') <- move :: stacks.(to');
         do_action stacks (nb-1) from to_

let rec split_on_n n list = match n, list with
  | 0, _ -> [], list
  | _, t::q -> let hd, tl = split_on_n (n-1) q in
               t::hd, tl
  | _, [] -> failwith "N larger than list"

let do_action2 stacks nb from to_ =
  let from' = from - 1 in
  let to' = to_ - 1 in
  let moved, stay = split_on_n nb stacks.(from') in
  stacks.(from') <- stay;
  stacks.(to') <- moved @ stacks.(to')

let rec move array =
  try
    let line = read_line () in
    Scanf.sscanf line "move %d from %d to %d" (do_action2 array);
    move array
  with
    End_of_file -> ()

let top_crates stacks =
  Array.iter (function
    | t::_ -> Format.printf "%c" t
    | [] -> failwith "Empty stack") stacks

let () =
  let stacks = make_stacks [] in
  let stacks = rotate_stacks stacks in
  move stacks; top_crates stacks; Format.printf "\n"
