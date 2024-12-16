open Utils

type t = int * int

let compare (x,y) (x',y') =
  let cmp = Int.compare x x' in
  if cmp = 0 then Int.compare y y' else cmp

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

let inverse (x,y) (x',y') = (y',-y), (-x',x)

module Set = Set.Make(struct type nonrec t = t let compare = compare end)
