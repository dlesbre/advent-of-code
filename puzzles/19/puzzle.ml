(* ==== Puzzle 19 : https://adventofcode.com/2022/day/19 ==== *)

type recipe = {
  ore: int;
  clay: int;
  obsidian: int;
}

type blueprint = {
  number: int;
  ore_robot: recipe;
  clay_robot: recipe;
  obsidian_robot: recipe;
  geode_robot: recipe;
}

type ressource = {
  ore_nb: int;      ore_robot_nb: int;
  clay_nb: int;     clay_robot_nb: int;
  obsidian_nb: int; obsidian_robot_nb: int;
  geode_nb: int;    geode_robot_nb: int;
  time: int;
}

let parse_recipe r =
  let words = String.split_on_char ' ' r in
  let rec runner r = function
    | nb::unit::rest ->
        let nb = int_of_string nb in
        let r = match unit with
          | "ore" -> { r with ore = nb }
          | "clay" -> { r with clay = nb }
          | "obsidian" -> { r with obsidian = nb }
          | s -> failwith ("unknown unit "^s)
        in
        if List.length rest = 0
        then r
        else runner r (List.tl rest) (* remove "and" *)
    | _ -> failwith "wrong format"
  in runner { ore=0; clay=0; obsidian=0} words



let parse_line line =
  Scanf.sscanf line "Blueprint %d: Each ore robot costs %s@. Each clay robot costs %s@. Each obsidian robot costs %s@. Each geode robot costs %s@."
    (fun number ore_recipe clay_recipe obsidian_recipe geode_recipe -> { number;
      ore_robot = parse_recipe ore_recipe;
      clay_robot = parse_recipe clay_recipe;
      obsidian_robot = parse_recipe obsidian_recipe;
      geode_robot = parse_recipe geode_recipe;
    })

(** Returns a list of all lines in the input, first line as hd *)
let rec read_all_lines acc =
  try
    let line = read_line () in
    let contents = parse_line line in
    read_all_lines (contents::acc)
  with End_of_file ->
    List.rev acc

let update r = {
    r with
    ore_nb = r.ore_nb + r.ore_robot_nb;
    clay_nb = r.clay_nb + r.clay_robot_nb;
    obsidian_nb = r.obsidian_nb + r.obsidian_robot_nb;
    geode_nb = r.geode_nb + r.geode_robot_nb;
    time = r.time + 1;
  }

let ( <== ) recipe ressource =
    recipe.ore <= ressource.ore_nb &&
    recipe.clay <= ressource.clay_nb &&
    recipe.obsidian <= ressource.obsidian_nb

let ( -- ) ressource recipe = {
  ressource with
  ore_nb = ressource.ore_nb - recipe.ore;
  clay_nb = ressource.clay_nb - recipe.clay;
  obsidian_nb = ressource.obsidian_nb - recipe.obsidian;
}

let max4 a b c d = max (max a b) (max c d)

(* the maximum of each intermediate unit for any recipe
   This is the maximum number of intermediate robots we ever need to build *)
let max_recipe blueprint = {
  ore = max4
    blueprint.ore_robot.ore
    blueprint.clay_robot.ore
    blueprint.obsidian_robot.ore
    blueprint.geode_robot.ore;
  clay = max4
    blueprint.ore_robot.clay
    blueprint.clay_robot.clay
    blueprint.obsidian_robot.clay
    blueprint.geode_robot.clay;
  obsidian = max4
    blueprint.ore_robot.obsidian
    blueprint.clay_robot.obsidian
    blueprint.obsidian_robot.obsidian
    blueprint.geode_robot.obsidian;
}



let cant_beat_max tmax r current_max =
  let t_remain = tmax - r.time in
  let geodes = r.geode_nb + t_remain*r.geode_robot_nb + t_remain*(t_remain+1)/2 in
  geodes <= current_max

let pp_resource fmt r =
  Format.fprintf fmt "{
  ore_nb = %d;      ore_robot_nb = %d;
  clay_nb = %d;     clay_robot_nb = %d;
  obsidian_nb = %d; obsidian_robot_nb = %d;
  geode_nb = %d;    geode_robot_nb = %d;
  time = %d;
}@." r.ore_nb r.ore_robot_nb r.clay_nb r.clay_robot_nb r.obsidian_nb r.obsidian_robot_nb
    r.geode_nb r.geode_robot_nb r.time

let maxr a b = if a.geode_nb > b.geode_nb then a else b

module RessourceSet = Set.Make(struct
  type t = ressource
  let compare = compare
end)

let rec max_geodes tmax blueprint max_r current_max seen = function
  | [] -> current_max
  | r::rs ->
      if r.time = tmax || cant_beat_max tmax r current_max.geode_nb || RessourceSet.mem r seen
      then max_geodes tmax blueprint max_r (maxr r current_max) seen rs
      else
        let seen = RessourceSet.add r seen in
        let r' = update r in
        let build_geode = blueprint.geode_robot <== r in
        if build_geode then
          max_geodes tmax blueprint max_r current_max seen
          ({(r'--blueprint.geode_robot) with geode_robot_nb = r'.geode_robot_nb+1 }::rs)
        else
        let build_ore = blueprint.ore_robot <== r && r.ore_robot_nb < max_r.ore in
        let build_clay = blueprint.clay_robot <== r && r.clay_robot_nb < max_r.clay in
        let build_obsidian = blueprint.obsidian_robot <== r && r.obsidian_robot_nb < max_r.obsidian in
        let rs =
          if build_ore
          then {(r'--blueprint.ore_robot) with ore_robot_nb = r'.ore_robot_nb+1 }::rs
          else rs in
        let rs =
          if build_clay
          then {(r'--blueprint.clay_robot) with clay_robot_nb = r'.clay_robot_nb+1 }::rs
          else rs in
        let rs =
          if build_obsidian
          then {(r'--blueprint.obsidian_robot) with obsidian_robot_nb = r'.obsidian_robot_nb+1 }::rs
          else rs in
        (* if build_ore || build_clay || build_obsidian || build_geode
          then max_geodes blueprint max_r current_max rs (* if you can build a robot, you must do so *)
          else *) max_geodes tmax blueprint max_r current_max seen (r'::rs)

let ressource_init = {
  ore_nb = 0;      ore_robot_nb = 1;
  clay_nb = 0;     clay_robot_nb = 0;
  obsidian_nb = 0; obsidian_robot_nb = 0;
  geode_nb = 0;    geode_robot_nb = 0;
  time = 1;
}

let part1 () =
  let blueprints = read_all_lines [] in
  let score = List.fold_left (fun s bp ->
      let m = max_geodes 25 bp (max_recipe bp) ressource_init RessourceSet.empty [ressource_init] in
      let m' = m.geode_nb * bp.number in
      Format.printf "Blueprint %d : max of %d geodes (score %d)@."
      bp.number m.geode_nb m';
      s + m'
    ) 0 blueprints in
  Format.printf "%d@." score

let rec list_firstn n l = match n, l with
  |0, _
  |_, [] -> []
  |_, l::ls -> l::(list_firstn (n-1) ls)

let part2 () =
  let blueprints = list_firstn 3 (read_all_lines []) in
  let score = List.fold_left (fun s bp ->
      let m = max_geodes 33 bp (max_recipe bp) ressource_init RessourceSet.empty [ressource_init] in
      let m' = m.geode_nb * bp.number in
      Format.printf "Blueprint %d : max of %d geodes (score %d)@."
      bp.number m.geode_nb m';
      s * m.geode_nb
    ) 1 blueprints in
  Format.printf "%d@." score

let () = part2 ()
