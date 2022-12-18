(* ==== Puzzle 18 : https://adventofcode.com/2022/day/18 ==== *)

type direction = X_Plus | X_Minus | Y_Plus | Y_Minus | Z_Plus | Z_Minus

let flip = function
  | X_Plus ->  X_Minus
  | X_Minus -> X_Plus
  | Y_Plus ->  Y_Minus
  | Y_Minus -> Y_Plus
  | Z_Plus ->  Z_Minus
  | Z_Minus -> Z_Plus

module Vec3 : sig
  type t

  val add : t -> t -> t
  val sub : t -> t -> t
  val of_direction : direction -> t
  val of_ints : int -> int -> int -> t
  val compare : t -> t -> int

  module Space : sig (* Wrappers for 3D arrays accessed by 3D vector *)
    type 'a space
    val init : t -> t -> (t -> 'a) -> 'a space

    val get : 'a space -> t -> 'a option (* None when out of bounds *)
    val set : 'a space -> t -> 'a -> unit
  end

  val x : t -> int
  val y : t -> int
  val z : t -> int
end = struct
  type t = int * int * int

  let add (x,y,z) (x',y',z') = x+x', y+y', z+z'
  let sub (x,y,z) (x',y',z') = x-x', y-y', z-z'

  let of_direction = function
    | X_Plus ->  ( 1, 0, 0)
    | X_Minus -> (-1, 0, 0)
    | Y_Plus ->  ( 0, 1, 0)
    | Y_Minus -> ( 0,-1, 0)
    | Z_Plus ->  ( 0, 0, 1)
    | Z_Minus -> ( 0, 0,-1)

  let of_ints x y z = (x,y,z)

  let compare = compare

  let x (x,_,_) = x
  let y (_,y,_) = y
  let z (_,_,z) = z

  module Space = struct
    type 'a space = {
      values: 'a array array array;
      offset: t;
      size: t;
    }

    let init min max f =
      let size = sub max min in
      {
        size; offset=min;
        values=    Array.init (x size) (
          fun x -> Array.init (y size) (
          fun y -> Array.init (z size) (
          fun z -> f (add (of_ints x y z) min)
        )))
      }

    let in_bounds space t =
      let (x',y',z') = sub t space.offset in
      0 <= x' && x' < x space.size &&
      0 <= y' && y' < y space.size &&
      0 <= z' && z' < z space.size

    let get space t =
      if in_bounds space t then
        let (x',y',z') = sub t space.offset in
        Some space.values.(x').(y').(z')
      else None

    let set space t v =
      let (x',y',z') = sub t space.offset in
      space.values.(x').(y').(z') <- v
  end
end

let parse_line line = Scanf.sscanf line "%d,%d,%d" Vec3.of_ints

(** Returns a list of all lines in the input, first line as hd *)
let rec read_all_lines acc =
  try
    let line = read_line () in
    let contents = parse_line line in
    read_all_lines (contents::acc)
  with End_of_file ->
    List.rev acc

module Vec3Set = Set.Make(Vec3)

(* list of all direction to quickly look at adjacent cubes *)
let directions = List.map Vec3.of_direction [X_Plus; X_Minus; Y_Plus; Y_Minus; Z_Plus; Z_Minus]

(* free surface area of the cube at x, where set is the set of occupied cubes *)
let free_surface set x =
  List.fold_left (fun nb dir ->
    if Vec3Set.mem (Vec3.add x dir) set then nb else nb+1
  ) 0 directions

let part1 () =
  let nodes = read_all_lines [] in
  let nodes_set = Vec3Set.of_list nodes in
  let surface = List.fold_left (fun total vec3 ->
      total + free_surface nodes_set vec3
    ) 0 nodes in
  Format.printf "%d@." surface

(* == part 2 == *)

let list_max ?(max=max) f m =
  List.fold_left (fun acc x -> max acc (f x)) m

type space = Air | Water | Lava

(* Depth first search *)
let rec propagate_water array pos =
  match pos with
  | [] -> array
  | p::ps ->
      match Vec3.Space.get array p with
      | None
      | Some Lava
      | Some Water -> propagate_water array ps
      | Some Air ->
          Vec3.Space.set array p Water;
          propagate_water array (List.map (Vec3.add p) directions @ ps)

let surface_exposed_to_water array node =
  List.fold_left (fun acc x ->
      if Vec3.Space.get array (Vec3.add node x) = Some Water
      then acc + 1
      else acc
    ) 0 directions

let part2 () =
  let nodes = read_all_lines [] in
  let set = Vec3Set.of_list nodes in
  (* find the cube containing all rocks, and expand it to ensure water can go around *)
  let max_x = list_max ~max:max Vec3.x min_int nodes + 2 in
  let min_x = list_max ~max:min Vec3.x max_int nodes - 2 in
  let max_y = list_max ~max:max Vec3.y min_int nodes + 2 in
  let min_y = list_max ~max:min Vec3.y max_int nodes - 2 in
  let max_z = list_max ~max:max Vec3.z min_int nodes + 2 in
  let min_z = list_max ~max:min Vec3.z max_int nodes - 2 in
  let space = Vec3.Space.init
    (Vec3.of_ints min_x min_y min_z)
    (Vec3.of_ints max_x max_y max_z)
    (fun t -> if Vec3Set.mem t set then Lava else Air) in
  (* propagate water from a corner *)
  let space = propagate_water space [Vec3.of_ints min_x min_y min_z] in
  let exposed = Vec3Set.fold (fun t acc -> acc + surface_exposed_to_water space t) set 0 in
  Format.printf "%d@." exposed

let () = part2 ()
