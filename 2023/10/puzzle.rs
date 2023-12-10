// ==== Puzzle 10 : https://adventofcode.com/2023/day/10 ====

#[derive(PartialEq, Eq, Clone, Copy)]
enum Direction {
    N,
    S,
    E,
    W,
}

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

#[derive(PartialEq, Eq)]
enum Tile {
    NS,
    EW,
    NE,
    NW,
    SE,
    SW,
    Empty,
    Start,
}

#[derive(Clone, Debug, PartialEq)]
struct P2 {
    x: usize,
    y: usize,
}

impl P2 {
    fn step(&self, dir: &Direction) -> P2 {
        match dir {
            Direction::N => P2 {
                x: self.x,
                y: self.y - 1,
            },
            Direction::S => P2 {
                x: self.x,
                y: self.y + 1,
            },
            Direction::E => P2 {
                x: self.x + 1,
                y: self.y,
            },
            Direction::W => P2 {
                x: self.x - 1,
                y: self.y,
            },
        }
    }
}

#[derive(PartialEq)]
enum Neighbors {
    Pair(Direction, Direction),
    Start,
    Empty,
}

impl Tile {
    fn from_char(chr: &char) -> Tile {
        match chr {
            '|' => Tile::NS,
            '-' => Tile::EW,
            'L' => Tile::NE,
            'J' => Tile::NW,
            '7' => Tile::SW,
            'F' => Tile::SE,
            '.' => Tile::Empty,
            'S' => Tile::Start,
            _ => panic!("Unknown tile"),
        }
    }

    fn neighbors(&self) -> Neighbors {
        match self {
            Tile::Empty => Neighbors::Empty,
            Tile::NS => Neighbors::Pair(Direction::N, Direction::S),
            Tile::EW => Neighbors::Pair(Direction::E, Direction::W),
            Tile::NE => Neighbors::Pair(Direction::N, Direction::E),
            Tile::NW => Neighbors::Pair(Direction::N, Direction::W),
            Tile::SE => Neighbors::Pair(Direction::S, Direction::E),
            Tile::SW => Neighbors::Pair(Direction::S, Direction::W),
            Tile::Start => Neighbors::Start,
        }
    }
}

impl Neighbors {
    fn next(&self, dir: &Direction) -> Option<Direction> {
        let dir = dir.flip();
        match self {
            Neighbors::Pair(l, r) if *l == dir => Some(*r),
            Neighbors::Pair(l, r) if *r == dir => Some(*l),
            _ => None,
        }
    }
}

fn parse_line(line: String) -> Vec<Tile> {
    line.chars().map(|c| Tile::from_char(&c)).collect()
}

fn read_lines() -> Vec<Vec<Tile>> {
    std::io::stdin()
        .lines()
        .filter_map(|x| x.ok())
        .map(parse_line)
        .collect()
}

fn find_start(lines: &Vec<Vec<Tile>>) -> P2 {
    let y_max = lines.len();
    let x_max = lines[0].len();
    for y in 0..y_max {
        for x in 0..x_max {
            if lines[y][x] == Tile::Start {
                return P2 { x: x + 1, y: y + 1 };
            }
        }
    }
    panic!("Not found");
}

enum Status {
    Unexplored,
    Loop(u32),
}

// fn explore(map: &Vec<Vec<Tile>>, status: &mut Vec<Vec<Status>>, todo: &mut VecDeque<P2>) {
//   match todo.pop_front() {
//     None => (),
//     Some(p) => {
//       if status[p.y-1][p.x-1] != Status::Unexplored { return; }
//       let node = map[p.y-1][p.x-1];
//       if node == Tile::Empty { ret}
//     }
//   }
// }

fn in_bounds(p: &P2, max: &P2) -> bool {
    p.x > 0 && p.y > 0 && p.x < max.x && p.y < max.y
}

// Follows a branch to its end
// Returns Some(len) if that end is start, None otherwise
// tail-recursive
fn follow_branch(
    map: &Vec<Vec<Tile>>,
    pos: P2,
    dir: Direction,
    steps: u32,
    max: &P2,
) -> Option<u32> {
    let npos = pos.step(&dir);
    if !in_bounds(&npos, max) {
        // out of bounds
        return None;
    }
    let node = &map[npos.y - 1][npos.x - 1];
    let neighbors = node.neighbors();
    if neighbors == Neighbors::Start {
        return Some(steps);
    }
    match neighbors.next(&dir) {
        None => None,
        Some(n) => follow_branch(map, npos, n, steps + 1, max),
    }
}
fn dfs(
    map: &Vec<Vec<Tile>>,
    status: &mut Vec<Vec<Status>>,
    pos: P2,
    dir: Direction,
    steps: u32,
    max: &P2,
) -> bool {
    let npos = pos.step(&dir);
    if !in_bounds(&npos, max) {
        // out of bounds
        return false;
    }
    let node = &map[npos.y - 1][npos.x - 1];
    let neighbors = node.neighbors();
    if neighbors == Neighbors::Start {
        return true;
    }
    let next = match neighbors.next(&dir) {
        None => {
            return false;
        }
        Some(n) => n,
    };
    match status[npos.y - 1][npos.x - 1] {
        //Status::DeadPath => false,
        Status::Loop(n) if (n <= steps) => true,
        _ => {
            // if *node == Tile::Empty {
            //     status[pos.y - 1][pos.x - 1] = Status::DeadPath;
            //     return false;
            // }
            status[pos.y - 1][pos.x - 1] = Status::Loop(steps);
            return dfs(map, status, npos, next, steps + 1, max);
        }
    }
}

fn main() {
    let input = read_lines();
    let start = find_start(&input);
    let max = P2 {
        x: input[0].len() + 1,
        y: input.len() + 1,
    };
    let mut status: Vec<Vec<_>> = input
        .iter()
        .map(|x| x.iter().map(|_| Status::Unexplored).collect())
        .collect();
    status[start.y - 1][start.x - 1] = Status::Loop(0);
    dfs(&input, &mut status, start.clone(), Direction::N, 0, &max);
    dfs(&input, &mut status, start.clone(), Direction::S, 0, &max);
    dfs(&input, &mut status, start.clone(), Direction::E, 0, &max);
    dfs(&input, &mut status, start, Direction::W, 0, &max);

    println!(
        "{:?}",
        status
            .iter()
            .map(|v| v
                .iter()
                .map(|x| match x {
                    Status::Loop(n) => *n,
                    _ => 0,
                })
                .max())
            .max()
    );
    for vec in status {
        for s in vec {
            match s {
                // Status::DeadPath => print!("_"),
                Status::Loop(n) => print!("{}", n),
                Status::Unexplored => print!("+"),
            }
        }
        println!("");
    }
}
