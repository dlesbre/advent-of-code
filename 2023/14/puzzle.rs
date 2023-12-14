// ==== Puzzle 14 : https://adventofcode.com/2023/day/14 ====

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Rock {
    Empty,
    Rounded,
    Square,
}

use Rock::*;

fn parse_rock(c: char) -> Rock {
    match c {
        'O' => Rounded,
        '#' => Square,
        '.' => Empty,
        _ => panic!("Unknown rock"),
    }
}

fn read_lines() -> Vec<Vec<Rock>> {
    std::io::stdin()
        .lines()
        .filter_map(|x| x.ok())
        .map(|c| c.chars().map(parse_rock).collect())
        .collect()
}

fn shift_south(grid: &mut Vec<Vec<Rock>>) {
    let l = grid.len();
    for i in 0..grid[0].len() {
        let mut offset = l - 1;
        for j in (0..l).rev() {
            match grid[j][i] {
                Empty => (),
                Square => offset = usize::saturating_sub(j, 1),
                Rounded => {
                    grid[j][i] = Empty;
                    grid[offset][i] = Rounded;
                    offset = usize::saturating_sub(offset, 1);
                }
            }
        }
    }
}

fn shift_east(grid: &mut Vec<Vec<Rock>>) {
    let l = grid[0].len();
    for i in 0..grid.len() {
        let mut offset = l - 1;
        for j in (0..l).rev() {
            match grid[i][j] {
                Empty => (),
                Square => offset = usize::saturating_sub(j, 1),
                Rounded => {
                    grid[i][j] = Empty;
                    grid[i][offset] = Rounded;
                    offset = usize::saturating_sub(offset, 1);
                }
            }
        }
    }
}

fn shift_north(grid: &mut Vec<Vec<Rock>>) {
    let l = grid.len();
    for i in 0..grid[0].len() {
        let mut offset = 0;
        for j in 0..l {
            match grid[j][i] {
                Empty => (),
                Square => offset = j + 1,
                Rounded => {
                    grid[j][i] = Empty;
                    grid[offset][i] = Rounded;
                    offset += 1;
                }
            }
        }
    }
}

fn shift_west(grid: &mut Vec<Vec<Rock>>) {
    let l = grid[0].len();
    for i in 0..grid.len() {
        let mut offset = 0;
        for j in 0..l {
            match grid[i][j] {
                Empty => (),
                Square => offset = j + 1,
                Rounded => {
                    grid[i][j] = Empty;
                    grid[i][offset] = Rounded;
                    offset += 1;
                }
            }
        }
    }
}

fn cycle(grid: &mut Vec<Vec<Rock>>) {
    shift_north(grid);
    shift_west(grid);
    shift_south(grid);
    shift_east(grid);
}

fn load(grid: &Vec<Vec<Rock>>) -> usize {
    grid.iter()
        .enumerate()
        .map(|(i, v)| v.iter().filter(|c| **c == Rounded).count() * (grid.len() - i))
        .sum()
}

use std::collections::HashMap;

fn final_iterations(n: usize, mut grid: Vec<Vec<Rock>>) -> usize {
    if n == 0 {
        return load(&grid);
    }
    cycle(&mut grid);
    return final_iterations(n - 1, grid);
}

fn billion_iterations(
    n: usize,
    grid: Vec<Vec<Rock>>,
    map: &mut HashMap<Vec<Vec<Rock>>, usize>,
) -> usize {
    match map.get(&grid) {
        Some(m) => {
            let cycle_len = n - m;
            let end = 1_000_000_000 - m;
            return final_iterations(end % cycle_len, grid);
        }
        None => (),
    }
    let mut next = grid.clone();
    cycle(&mut next);
    map.insert(grid, n);
    return billion_iterations(n + 1, next, map);
}

fn main() {
    let grid = read_lines();

    // Part 1
    let mut p1 = grid.clone();
    shift_north(&mut p1);
    let p1 = load(&p1);
    println!("Part 1 : {}", p1);

    // Part 2
    let mut map = HashMap::new();
    let p2 = billion_iterations(0, grid, &mut map);
    println!("Part 2 : {}", p2);
}
