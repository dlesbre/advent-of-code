<!-- LTeX: language=en -->

# Advent of Code

Puzzle solutions for [advent of code 2022](https://adventofcode.com/2022/), in OCaml,
and [advent of code 2023](https://adventofcode.com/2023/), in Rust.
Most run under 1s, all run under 5 minutes.

Building just requires a not too old [ocamlc](https://v2.ocaml.org/docs/install.html)
or [rustc](https://www.rust-lang.org/tools/install).
I tested it on **OCaml 4.14.0** but it should work with earlier versions as well
and **Rust 1.71.1**.

To compile and test on a perticular day's puzzle, use
```bash
make YEAR=2022 DAY=03 compile  # compiles
make YEAR=2022 DAY=03          # compiles and runs (part 2) on test date
make YEAR=2022 DAY=03 run      # compiles and runs on real data
```

**Note:** test and data files are not included in this repository for
[copyright reasons](https://adventofcode.com/about#faq_copying). In order to
run the solution (`make` or `make run`), go to the corresponding AOC puzzle and copy:
- the example input to `<year>/<day>/puzzle_test.txt`
- the true input to `<year>/<day>/puzzle_data.txt`
