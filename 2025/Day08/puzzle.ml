(* ==== Puzzle 08 : https://adventofcode.com/2025/day/08 ====  *)

type junction = { id:int; pos: int*int*int }

let euclidian_dist_squared (x1,y1,z1) (x2,y2,z2) =
  let dx = x1 - x2 in
  let dy = y1 - y2 in
  let dz = z1 - z2 in
  dx*dx + dy*dy + dz*dz

let pairwise_distances input =
  list_fold_pairs ~reflexive:false (fun map x y ->
    let dist = euclidian_dist_squared x.pos y.pos in
    (dist, (x,y)):: map
  ) [] input
  |> List.sort (fun (d1,_) (d2,_) -> Int.compare d1 d2)

exception LastConnection

module Union_Find = struct
  type node =
    | Child of int
    | Root of int (** size, i.e. number of children *)

  type t = node array
  let make n = Array.make n (Root 1)

  (** Returns root_id, cluster_size *)
  let rec find uf n = match Array.get uf n with
    | Child n' ->
        let (root, _) as r = find uf n' in
        uf.(n) <- Child root;
        r
    | Root size -> n, size

  let union uf a b =
    let a, size_a = find uf a in
    let b, size_b = find uf b in
    if a = b then () else
    if size_a > size_b then (
      uf.(b) <- Child a;
      uf.(a) <- Root (size_a + size_b);
    )
    else (
      uf.(a) <- Child b;
      uf.(b) <- Root (size_a + size_b);
    );
    if size_a + size_b >= Array.length uf then
      raise LastConnection
end

(**)


let preprocess input =
  let input = List.mapi (fun id s -> Scanf.sscanf s "%d,%d,%d" (fun x y z -> { id; pos =x,y,z })) input in
  let distances = pairwise_distances input in
  let clusters = Union_Find.make (List.length input) in
  clusters, distances

let nb_connections () = if !test then 10 else 1000

let pp fmt {id;pos=(x,y,z)} =
  Format.fprintf fmt "%d(%d,%d,%d)" id x y z

let rec join_smallest clusters distances = function
  | 0 -> Either.Left (clusters, distances)
  | n ->
      let _, (x,y) = List.hd distances in
      let distances = List.tl distances in
      match Union_Find.union clusters x.id y.id with
        | () -> join_smallest clusters distances (n-1)
        | exception LastConnection -> Either.Right(x,y)

type max3 = | Zero | One of int | Two of int * int | Three of int * int * int

let max3 elt = function
  | Zero -> One elt
  | One a -> if elt > a then Two(elt,a) else Two (a, elt)
  | Two (a,b) -> if elt > a then Three(elt,a,b)
                 else if elt > b then Three(a,elt,b)
                 else Three(a,b,elt)
  | Three (a,b,c) as acc ->
      if elt > a then Three(elt,a,b)
      else if elt > b then Three(a,elt,b)
      else if elt > c then Three(a,b,elt)
      else acc

let part1 (clusters, distances) =
  match join_smallest clusters distances (nb_connections ()) with
  | Right _ -> failwith "ran out of clusters in p1"
  | Left(clusters, distances) ->
  let max3 acc = function
    | Union_Find.Child _ -> acc
    | Union_Find.Root n -> max3 n acc
  in
  match Array.fold_left max3 Zero clusters with
  | Three(a,b,c) -> a*b*c, (clusters,distances)
  | _ -> failwith "Not enough clusters"

let part2 _ (clusters, distances) =
  match join_smallest clusters distances ((List.length distances) + 3) with
  | Left _ -> failwith "Did not merge all clusters"
  | Right({ pos=(x,_,_); _ }, { pos=(x',_,_); _ }) -> x * x'

let () = register_int_chained ~year:2025 ~day:08 ~preprocess ~part1 ~part2
