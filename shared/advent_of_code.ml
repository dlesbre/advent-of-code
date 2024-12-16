let usage_msg = "advent_of_code <year> <day> < input"

let year = ref None
let day = ref None

let int_of_string desc x =
  try int_of_string x
  with Failure _ -> raise (Arg.Bad ("Invalid number for "^desc^": "^x))

let anon_fun x =
  match !year with
  | None -> year:=Some (int_of_string "year" x)
  | _ -> match !day with
      | None -> let d = int_of_string "day" x in if d < 1 || d > 25 then
        raise (Arg.Bad ("Day should be between 1 and 25 inclusive. Got "^x));
        day := Some d
      | _ -> raise (Arg.Bad ("Extra argument "^x))

let () = Arg.parse [] anon_fun usage_msg
let () =
  match !year, !day with
  | None, _ | _, None -> Format.eprintf "Missing year/day"
  | Some year, Some day -> AOC.run_solution ~year ~day
