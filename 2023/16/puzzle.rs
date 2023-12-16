// ==== Puzzle 16 : https://adventofcode.com/2023/day/16 ====

#[derive(Clone, Copy, Debug)]
enum Tile {
    Empty,
    SplitterNS,
    SplitterEW,
    MirrorNEtoSW,
    MirrorNWtoSE,
}

use std::collections::VecDeque;

use Tile::*;

impl Tile {
    fn from_char(c: char) -> Tile {
        match c {
            '.' => Empty,
            '|' => SplitterNS,
            '-' => SplitterEW,
            '\\' => MirrorNWtoSE,
            '/' => MirrorNEtoSW,
            _ => panic!("Unknown tile"),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Direction {
    N = 0b0001,
    S = 0b0010,
    E = 0b0100,
    W = 0b1000,
}

struct DirectionSet(isize);

impl DirectionSet {
    const EMPTY: DirectionSet = DirectionSet(0);

    // fn of_direction(x: &Direction) -> Self {
    //     DirectionSet(*x as isize)
    // }

    fn is_empty(&self) -> bool {
        let DirectionSet(i) = self;
        *i == 0
    }

    fn add(&self, x: &Direction) -> Self {
        let DirectionSet(i) = self;
        DirectionSet(i | *x as isize)
    }

    fn contains(&self, dir: &Direction) -> bool {
        let DirectionSet(i) = self;
        i & (*dir as isize) != 0
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct P2 {
    x: usize,
    y: usize,
}

impl P2 {
    fn step(&self, dir: &Direction, max_x: usize, max_y: usize) -> Option<P2> {
        match dir {
            Direction::N if self.y > 0 => Some(P2 {
                x: self.x,
                y: self.y - 1,
            }),
            Direction::S if self.y + 1 < max_y => Some(P2 {
                x: self.x,
                y: self.y + 1,
            }),
            Direction::E if self.x + 1 < max_x => Some(P2 {
                x: self.x + 1,
                y: self.y,
            }),
            Direction::W if self.x > 0 => Some(P2 {
                x: self.x - 1,
                y: self.y,
            }),
            _ => None,
        }
    }
}

fn read_lines() -> Vec<Vec<Tile>> {
    std::io::stdin()
        .lines()
        .filter_map(|x| x.ok())
        .map(|x| x.chars().map(Tile::from_char).collect())
        .collect()
}

use Direction::*;

fn step(
    pos: &P2,
    dir: &Direction,
    max_x: usize,
    max_y: usize,
    worklist: &mut VecDeque<(P2, Direction)>,
) {
    if let Some(p) = pos.step(&dir, max_x, max_y) {
        worklist.push_back((p, *dir));
    }
}

fn propagate(
    grid: &Vec<Vec<Tile>>,
    energized: &mut Vec<Vec<DirectionSet>>,
    mut worklist: VecDeque<(P2, Direction)>,
) {
    let max_y = grid.len();
    let max_x = grid[0].len();

    match worklist.pop_front() {
        None => (),
        Some((pos, dir)) => {
            if energized[pos.y][pos.x].contains(&dir) {
                return propagate(grid, energized, worklist);
            }
            energized[pos.y][pos.x] = energized[pos.y][pos.x].add(&dir);
            // println!("At {:?}, {:?}, {:?}", pos, dir, grid[pos.y][pos.x]);
            match (grid[pos.y][pos.x], dir) {
                (Empty, _) | (SplitterEW, E | W) | (SplitterNS, N | S) => {
                    step(&pos, &dir, max_x, max_y, &mut worklist)
                }

                (SplitterEW, N | S) => {
                    step(&pos, &E, max_x, max_y, &mut worklist);
                    step(&pos, &W, max_x, max_y, &mut worklist);
                }
                (SplitterNS, W | E) => {
                    step(&pos, &N, max_x, max_y, &mut worklist);
                    step(&pos, &S, max_x, max_y, &mut worklist);
                }
                (MirrorNEtoSW, N) => step(&pos, &E, max_x, max_y, &mut worklist),
                (MirrorNEtoSW, W) => step(&pos, &S, max_x, max_y, &mut worklist),
                (MirrorNEtoSW, S) => step(&pos, &W, max_x, max_y, &mut worklist),
                (MirrorNEtoSW, E) => step(&pos, &N, max_x, max_y, &mut worklist),
                (MirrorNWtoSE, N) => step(&pos, &W, max_x, max_y, &mut worklist),
                (MirrorNWtoSE, W) => step(&pos, &N, max_x, max_y, &mut worklist),
                (MirrorNWtoSE, S) => step(&pos, &E, max_x, max_y, &mut worklist),
                (MirrorNWtoSE, E) => step(&pos, &S, max_x, max_y, &mut worklist),
            }
            return propagate(grid, energized, worklist);
        }
    }
    return;
}

fn solve(
    grid: &Vec<Vec<Tile>>,
    pos: P2,
    dir: Direction,
    energized: &mut Vec<Vec<DirectionSet>>,
) -> usize {
    for v in &mut *energized {
        for x in &mut *v {
            *x = DirectionSet::EMPTY;
        }
    }
    let mut worklist = VecDeque::new();
    worklist.push_back((pos, dir));
    propagate(grid, energized, worklist);
    energized
        .iter()
        .map(|v| v.iter().filter(|x| !x.is_empty()).count())
        .sum()
}

fn main() {
    let lines = read_lines();
    let mut energized: Vec<Vec<DirectionSet>> = lines
        .iter()
        .map(|x| x.iter().map(|_| DirectionSet::EMPTY).collect())
        .collect();

    let p1 = solve(&lines, P2 { x: 0, y: 0 }, E, &mut energized);

    // for line in energized {
    //     for v in line {
    //         if v.is_empty() {
    //             print!(".");
    //         } else {
    //             print!("#");
    //         }
    //     }
    //     println!("");
    // }
    println!("Part 1 : {}", p1);

    let mut max = 0;
    let y_max = lines.len() - 1;
    for x in 0..lines[0].len() {
        max = std::cmp::max(max, solve(&lines, P2 { x, y: 0 }, S, &mut energized));
        max = std::cmp::max(max, solve(&lines, P2 { x, y: y_max }, N, &mut energized));
    }
    let x_max = lines[0].len() - 1;
    for y in 0..lines.len() {
        max = std::cmp::max(max, solve(&lines, P2 { x: 0, y }, E, &mut energized));
        max = std::cmp::max(max, solve(&lines, P2 { x: x_max, y }, W, &mut energized));
    }
    println!("Part 2 : {}", max);
}
