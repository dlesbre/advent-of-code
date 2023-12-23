// // ==== Puzzle 23 : https://adventofcode.com/2023/day/23 ====

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct P2 {
    x: usize,
    y: usize,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, PartialOrd, Ord)]
enum Direction {
    N = 0,
    E = 1,
    S = 2,
    W = 3,
}

#[derive(PartialEq, Eq)]
enum Tile {
    Forest,
    Path,
    Slope(Direction),
}

use Direction::*;
use Tile::*;

impl Direction {
    fn flip(&self) -> Direction {
        match self {
            Direction::N => Direction::S,
            Direction::S => Direction::N,
            Direction::E => Direction::W,
            Direction::W => Direction::E,
        }
    }
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

fn parse_tile(s: char) -> Tile {
    match s {
        '.' => Path,
        '#' => Forest,
        '>' => Slope(E),
        '^' => Slope(N),
        'v' => Slope(S),
        '<' => Slope(W),
        _ => panic!("Unknown tile"),
    }
}

fn read_lines() -> Vec<Vec<Tile>> {
    std::io::stdin()
        .lines()
        .filter_map(|x| x.ok())
        .map(|v| v.chars().map(parse_tile).collect())
        .collect()
}

fn find_path(line: &Vec<Tile>) -> usize {
    for (i, x) in line.iter().enumerate() {
        if *x == Path {
            return i;
        }
    }
    panic!("No path on line")
}

use std::collections::{HashSet, VecDeque};

// Part 1 : naive DFS, very heavy memory usage if used directly for part2
// as the set of visited states if fully copied at each intersection
fn explore(
    grid: &Vec<Vec<Tile>>,
    slopes: bool,
    max_x: usize,
    max_y: usize,
    start: P2,
    end: P2,
) -> usize {
    let directions = vec![Direction::N, Direction::S, Direction::E, Direction::W];
    let dirs = [vec![N], vec![E], vec![S], vec![W]];

    let mut visited = Vec::with_capacity(max_y);
    for i in 0..max_y {
        visited.push(Vec::with_capacity(max_x));
        for _ in 0..max_x {
            visited[i].push(false);
        }
    }

    let mut worklist = VecDeque::new();
    worklist.push_back((start, visited, 0));
    let mut max = 0;
    while let Some((mut pos, mut visited, mut steps)) = worklist.pop_front() {
        loop {
            if pos == end {
                max = std::cmp::max(max, steps);
                break;
            }
            let mut next_steps = Vec::new();
            let dirs = match grid[pos.y][pos.x] {
                Slope(d) if slopes => &dirs[d as usize],
                _ => &directions,
            };
            for dir in dirs {
                match pos.step(1, &dir, max_x, max_y) {
                    Some(next) if grid[next.y][next.x] != Forest && !visited[next.y][next.x] => {
                        next_steps.push(next)
                    }
                    _ => (),
                }
            }
            if next_steps.len() == 0 {
                break;
            }
            visited[pos.y][pos.x] = true;
            steps += 1;
            while next_steps.len() > 1 {
                let next = next_steps.pop().unwrap();
                worklist.push_back((next, visited.clone(), steps));
            }
            pos = next_steps.pop().unwrap();
        }
    }
    return max;
}

const DIRECTIONS: [Direction; 4] = [Direction::N, Direction::S, Direction::E, Direction::W];

// Returns the next intersection (or dead end), and distance to it
fn next_intersection(
    grid: &Vec<Vec<Tile>>,
    mut pos: P2,
    mut dir: Direction,
    max_x: usize,
    max_y: usize,
) -> (P2, usize) {
    let mut steps = 1;
    // println!("Propagating from {:?} via {:?}", pos, dir);
    loop {
        let flipped = dir.flip();
        let mut next_pos = None;
        for dir in DIRECTIONS {
            if dir == flipped {
                continue;
            }
            match pos.step(1, &dir, max_x, max_y) {
                Some(next) if grid[next.y][next.x] != Forest => {
                    if next_pos == None {
                        next_pos = Some((next, dir))
                    } else {
                        // println!("Mulitple dirs");
                        return (pos, steps);
                    }
                }
                _ => (),
            }
        }
        match next_pos {
            None => {
                // println!("No dirs");
                return (pos, steps);
            }
            Some((next, direction)) => {
                pos = next;
                dir = direction;
                // println!("      going {:?} via {:?}", pos, dir);
                steps += 1;
            }
        }
    }
}

use std::collections::HashMap;

type Point = usize;

// Returns a vector such that v[i].0 are intersection
// And v[i].1 is a list of (adjacent intersections, distance)
// Also returns end id
fn map_of_intersections(
    grid: &Vec<Vec<Tile>>,
    start: P2,
    end: P2,
    max_x: usize,
    max_y: usize,
) -> (Vec<Vec<(Point, usize)>>, Point) {
    let mut worklist = VecDeque::new();
    let mut intersections_ids = HashMap::new();
    let mut intersections = Vec::new();
    let mut seen = HashSet::new();

    worklist.push_back((start, 0));
    intersections_ids.insert(start, 0);
    intersections.push(Vec::new());
    seen.insert(0);

    while let Some((pos, id)) = worklist.pop_front() {
        // println!("Seeing intersection {:?}", pos);
        for dir in DIRECTIONS {
            match pos.step(1, &dir, max_x, max_y) {
                Some(p) if grid[p.y][p.x] != Forest => {
                    // println!("Going {:?}", dir);
                    let (p, dist) = next_intersection(grid, p, dir, max_x, max_y);
                    let id_p = match intersections_ids.get(&p) {
                        Some(i) => *i,
                        None => {
                            let i = intersections.len();
                            intersections.push(Vec::new());
                            intersections_ids.insert(p, i);
                            i
                        }
                    };
                    intersections[id].push((id_p, dist));
                    if !seen.contains(&id_p) {
                        seen.insert(id_p);
                        worklist.push_back((p, id_p));
                    }
                    // sleep(Duration::from_secs(1));
                }
                _ => (),
            }
        }
    }
    return (intersections, *intersections_ids.get(&end).unwrap());
}

#[derive(Clone, Copy)]
struct BoolSet(u64);

impl BoolSet {
    #[inline]
    fn get(&self, pos: usize) -> bool {
        let BoolSet(set) = *self;
        set & (1 << pos) != 0
    }

    #[inline]
    fn set(&mut self, pos: usize) {
        let BoolSet(set) = self;
        *set |= 1 << pos;
    }

    #[inline]
    fn unset(&mut self, pos: usize) {
        let BoolSet(set) = self;
        *set &= !(1 << pos);
    }
}

fn dfs(
    intersections: &Vec<Vec<(Point, usize)>>,
    end_id: Point,
    pos: Point,
    length: usize,
    max_path: &mut Vec<usize>,
    mut visited: BoolSet,
) {
    max_path[pos] = std::cmp::max(max_path[pos], length);
    if pos == end_id {
        return;
    }
    visited.set(pos);
    for (point, dist) in &intersections[pos] {
        if !visited.get(*point) {
            dfs(
                intersections,
                end_id,
                *point,
                length + *dist,
                max_path,
                visited,
            );
        }
    }
    visited.unset(pos);
}

fn main() {
    let grid = read_lines();

    let max_y = grid.len();
    let max_x = grid[0].len();

    let start = P2 {
        x: find_path(&grid[0]),
        y: 0,
    };
    let end = P2 {
        x: find_path(&grid[max_y - 1]),
        y: max_y - 1,
    };

    let p1 = explore(&grid, true, max_x, max_y, start, end);
    println!("Part 1 : {}", p1);

    let (intersections, end_id) = map_of_intersections(&grid, start, end, max_x, max_y);

    let mut max_path = Vec::with_capacity(intersections.len());
    for _ in &intersections {
        max_path.push(0);
    }

    let (pre_end_id, steps) = intersections[end_id][0];
    dfs(
        &intersections,
        pre_end_id,
        0,
        steps,
        &mut max_path,
        BoolSet(0),
    );
    println!("Part 2 : {}", max_path[pre_end_id]);
}
