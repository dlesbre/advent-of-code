open Utils

type t = int * int

let compare (x,y) (x',y') =
  let cmp = Int.compare x x' in
  if cmp = 0 then Int.compare y y' else cmp

let of_string str = Scanf.sscanf str ("%d,%d") (fun a b -> (a,b))

let in_box (x,y) ~low:(x',y') ~high:(x'',y'') =
  x' <= x && x <= x'' && y' <= y && y <= y''

let bounding_box = function
  | [] -> failwith "bounding_box of empty list"
  | (x,y)::r ->
      let (xm, ym, xM, yM) =
        List.fold_left
        (fun (x1,y1,x2,y2) (x,y) -> (min x1 x, min y1 y, max x2 x, max y2 y))
        (x,y,x,y) r
      in (xm, ym), (xM, yM)

let intersect_x_line ~x ~y1 ~y2 ~low:(xl,yl) ~high:(xh,yh) =
  let y1, y2 = min y1 y2, max y1 y2 in
  xl <= x && x <= xh && y1 <= yh && yl <= y2

let intersect_y_line ~y ~x1 ~x2 ~low:(xl,yl) ~high:(xh,yh) =
  let x1, x2 = min x1 x2, max x1 x2 in
  yl <= y && y <= yh && x1 <= xh && xl <= x2

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
