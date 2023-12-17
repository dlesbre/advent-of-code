// ==== Puzzle 17 : https://adventofcode.com/2023/day/17 ====

use std::collections::BTreeSet;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct P2 {
    x: usize,
    y: usize,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, PartialOrd, Ord)]
enum Direction {
    S = 0,
    E = 1,
    W = 3,
    N = 2,
}

impl P2 {
    fn step(&self, amount: usize, dir: &Direction, max_x: usize, max_y: usize) -> Option<P2> {
        match dir {
            Direction::N if self.y >= amount => Some(P2 {
                x: self.x,
                y: self.y - amount,
            }),
            Direction::S if self.y + amount < max_y => Some(P2 {
                x: self.x,
                y: self.y + amount,
            }),
            Direction::E if self.x + amount < max_x => Some(P2 {
                x: self.x + amount,
                y: self.y,
            }),
            Direction::W if self.x >= amount => Some(P2 {
                x: self.x - amount,
                y: self.y,
            }),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Head {
    score: u32,
    pos: P2,
    dir: Direction,
    // amount: u8,
}

fn read_lines() -> Vec<Vec<u32>> {
    std::io::stdin()
        .lines()
        .filter_map(|x| x.ok())
        .map(|x| x.chars().map(|c| c.to_digit(10).unwrap()).collect())
        .collect()
}

fn step(
    grid: &Vec<Vec<u32>>,
    pos: &P2,
    dir: &Direction,
    amount_min: usize,
    amount_max: usize,
    mut score: u32,
    max_x: usize,
    max_y: usize,
    worklist: &mut BTreeSet<Head>,
) {
    for amount in 1..=amount_max {
        //println!("{}", amount);
        if let Some(p) = pos.step(amount, &dir, max_x, max_y) {
            score += grid[p.y][p.x];
            if amount >= amount_min {
                worklist.insert(Head {
                    pos: p,
                    dir: *dir,
                    // amount: (amount - amount_min).try_into().unwrap(),
                    score,
                });
            }
        } else {
            return;
        }
    }
}

use Direction::*;

const VISITED: usize = 2;

fn bfs(
    grid: &Vec<Vec<u32>>,
    visited: &mut Vec<Vec<[u32; VISITED]>>,
    worklist: &mut BTreeSet<Head>,
    max_x: usize,
    max_y: usize,
    amount_min: usize,
    amount_max: usize,
) {
    while let Some(hd) = worklist.pop_first() {
        //println!("At {:?}, {}", hd.pos, hd.amount);
        if visited[hd.pos.y][hd.pos.x][hd.dir as usize & 1] <= hd.score {
            continue;
        }
        visited[hd.pos.y][hd.pos.x][hd.dir as usize & 1] = hd.score;
        if hd.pos.y == max_y - 1 && hd.pos.x == max_x - 1 {
            break;
        }
        match hd.dir {
            N | S => {
                step(
                    grid, &hd.pos, &E, amount_min, amount_max, hd.score, max_x, max_y, worklist,
                );
                step(
                    grid, &hd.pos, &W, amount_min, amount_max, hd.score, max_x, max_y, worklist,
                );
            }
            E | W => {
                step(
                    grid, &hd.pos, &N, amount_min, amount_max, hd.score, max_x, max_y, worklist,
                );
                step(
                    grid, &hd.pos, &S, amount_min, amount_max, hd.score, max_x, max_y, worklist,
                );
            }
        }
    }
}

fn solve(grid: &Vec<Vec<u32>>, amount_min: usize, amount_max: usize) -> u32 {
    let mut visited: Vec<Vec<[u32; VISITED]>> = grid
        .iter()
        .map(|v| v.iter().map(|_| [u32::MAX; VISITED]).collect())
        .collect();

    let mut worklist = BTreeSet::new();
    let max_x = grid[0].len();
    let max_y = grid.len();
    step(
        &grid,
        &P2 { x: 0, y: 0 },
        &E,
        amount_min,
        amount_max,
        0,
        max_x,
        max_y,
        &mut worklist,
    );
    step(
        &grid,
        &P2 { x: 0, y: 0 },
        &S,
        amount_min,
        amount_max,
        0,
        max_x,
        max_y,
        &mut worklist,
    );
    bfs(
        grid,
        &mut visited,
        &mut worklist,
        max_x,
        max_y,
        amount_min,
        amount_max,
    );
    *visited[max_y - 1][max_x - 1].iter().min().unwrap()
}

fn main() {
    let grid = read_lines();

    let p1 = solve(&grid, 1, 3);
    println!("Part 1 : {}", p1);

    let p2 = solve(&grid, 4, 10);
    println!("Part 2 : {}", p2);
}
