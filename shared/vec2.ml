open Utils

type t = int * int

let compare (x,y) (x',y') =
  let cmp = Int.compare x x' in
  if cmp = 0 then Int.compare y y' else cmp

let of_string str = Scanf.sscanf str ("%d,%d") (fun a b -> (a,b))

let in_box (x,y) ~low:(x',y') ~high:(x'',y'') =
  x' <= x && x <= x'' && y' <= y && y <= y''

let ( +| ) (x,y) (x',y') = x+x', y+y'
let ( -| ) (x,y) (x',y') = x-x', y-y'
let ( ~| ) (x,y) = (-x,-y)
let ( *| ) n (x,y) = n*x, n*y
let ( %| ) (x,y) (x',y') = (x % x', y % y')

let norm_manhattan (x,y) = abs x + abs y
let dist_manhattan a b = a -| b |> norm_manhattan

let pp fmt (x,y) = Format.fprintf fmt "(%d,%d)" x y

let dot (x,y) (x',y') = x*x' + y*y'

let det (x,y) (x',y') = x*y' - y*x'

(* Shoelace formula:
   2*A = sum (for i = 1 to N) x_i * (y_[i+1] - y_[i-1])
   Note that y_[i+1] / y_[i-1] may wrap around, we solve this by re-adding
   the last two elements at the front of the list
*)
let rec last_two = function
  | [a;b] -> a,b
  | [] | [_] -> failwith "list too short"
  | _::rest -> last_two rest


let double_area polygon =
  let a, b = last_two polygon in
  let rec aux sum = function
    | (_,y)::((x,_)::(_,y')::_ as rest) -> aux (sum + x*(y'-y)) rest
    | _ -> sum
  in aux 0 (a::b::polygon)


let inverse (x,y) (x',y') = (y',-y), (-x',x)

module Set = Set.Make(struct type nonrec t = t let compare = compare end)
