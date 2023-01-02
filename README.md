# Advent of Code

Puzzle solutions for [advent of code 2022](https://adventofcode.com/2022/), in OCaml.
Most run under 1s, all run under 5 minutes.

Building just requires a not too old [ocamlc](https://v2.ocaml.org/docs/install.html). 
I tested it on 4.14.0 but it should work with earlier versions as well.

To compile and test on a perticular day's puzzle, use
```bash
make DAY=03          # compiles
make DAY=03 run      # compiles and runs (part 2)
make DAY=03 run-test # compiles and runs on test data
```
