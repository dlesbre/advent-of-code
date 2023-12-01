(* ==== Puzzle 07 : https://adventofcode.com/2022/day/7 ==== *)

module StringMap = Map.Make(struct
  type t = string
  let compare = compare
end)

type directory = {
  mutable children: file StringMap.t;
  mutable size: int;
  parent: directory option;
}
and file =
  | Dir of directory
  | File of int

let string_suffix str start =
  String.sub str start (String.length str - start)

let rec parse_ls cwd found = function
  | [] -> found, []
  | t::q ->
      if String.starts_with ~prefix:"$ " t then
        found, t::q
      else
        let name, file =
          if String.starts_with ~prefix:"dir " t
          then string_suffix t 4, Dir { children=StringMap.empty; size=0; parent=Some cwd }
          else Scanf.sscanf t "%d %s" (fun s n -> n, File(s)) in
        parse_ls cwd (StringMap.add name file found) q

let root = { children=StringMap.empty; size=0; parent=None }

let rec parse_commands cwd lines =
  if lines = [] then () else
  let cmd = List.hd lines in
  let out = List.tl lines in
  if String.starts_with ~prefix:"$ cd" cmd then
    match string_suffix cmd 5 with
    | "/" -> parse_commands root out
    | ".." -> parse_commands (Option.value cwd.parent ~default:root) out
    | dir ->
        match StringMap.find dir cwd.children with
        | File n -> failwith "cd into a file!"
        | Dir d -> parse_commands d out
  else if cmd = "$ ls" then
    let files, out = parse_ls cwd StringMap.empty out in
    cwd.children <- files;
    parse_commands cwd out

let rec make_size cwd =
  let size = StringMap.fold (fun _ file size ->
      size + match file with
      | File n -> n
      | Dir d -> make_size d
    ) cwd.children 0 in
  cwd.size <- size; size

let rec find_smaller_than n cwd =
  let acc = StringMap.fold (fun _ file acc ->
    match file with
    | Dir d -> acc + find_smaller_than n d
    | _ -> acc
  ) cwd.children 0 in
  if cwd.size <= n then acc + cwd.size else acc

let rec read_all_inputs acc =
  try
    read_all_inputs (read_line ()::acc)
  with End_of_file ->
    List.rev acc

let rec print_tree depth cwd =
  StringMap.iter (fun name file ->
    match file with
    | File n -> Format.printf "%sfile \"%s\" : %d\n" depth name n
    | Dir d -> Format.printf "%sDir \"%s\" : %d\n" depth name d.size;
        print_tree (depth^"  ") d
    ) cwd.children

let part1 () =
  let cmds = read_all_inputs [] in
  let () = parse_commands root cmds in
  let _ = make_size root in
  let n = find_smaller_than 100000 root in
  Format.printf "%d\n" n

let rec find_smallest_larger_than n cwd =
  if cwd.size >= n then Some (
    StringMap.fold (fun _ file acc ->
      match file with
      | File _ -> acc
      | Dir d -> match find_smallest_larger_than n d with
          | Some x -> min x acc
          | _ -> acc
    ) cwd.children cwd.size)
  else None

let part1 () =
  let cmds = read_all_inputs [] in
  let () = parse_commands root cmds in
  let _ = make_size root in
  let free_space = 70000000 - root.size in
  let required = 30000000 - free_space in
  let n = find_smallest_larger_than required root in
  Format.printf "%d\n" (Option.get n)

let () = part1 ()
