# Advent of Code

Puzzle solutions for [advent of code 2022](https://adventofcode.com/2022/), in OCaml,
and [advent of code 2023](https://adventofcode.com/2023/), in rust.
Most run under 1s, all run under 5 minutes.

Building just requires a not too old [ocamlc](https://v2.ocaml.org/docs/install.html)
or [rustc](https://www.rust-lang.org/tools/install).
I tested it on OCamle 4.14.0 but it should work with earlier versions as well.

To compile and test on a perticular day's puzzle, use
```bash
make YEAR=2022 DAY=03 compile  # compiles
make YEAR=2022 DAY=03          # compiles and runs (part 2) on test date
make YEAR=2022 DAY=03 run      # compiles and runs on real data
```
