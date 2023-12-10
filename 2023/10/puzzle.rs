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

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
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
    pos: &P2,
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
        Some(n) => follow_branch(map, &npos, n, steps + 1, max),
    }
}

const DIRECTIONS: [Direction; 4] = [Direction::N, Direction::S, Direction::E, Direction::W];

fn find_loop(map: &Vec<Vec<Tile>>, pos: &P2, max: &P2) -> (Direction, u32) {
    for dir in DIRECTIONS {
        match follow_branch(map, pos, dir, 0, max) {
            Some(n) => return (dir, n),
            None => continue,
        }
    }
    panic!("No loop");
}

// Part 2

enum Status {
    Loop(Tile),
    NotLoop,
}

// Similar to follow branch, but simple since we now know the loop (no overflows)
// Marks all points of the loop in state
fn fill_loop(
    map: &Vec<Vec<Tile>>,
    pos: &P2,
    dir: Direction,
    points: &mut Vec<Vec<Status>>,
) -> Direction {
    let npos = pos.step(&dir);
    let node = &map[npos.y - 1][npos.x - 1];
    points[npos.y - 1][npos.x - 1] = Status::Loop(*node);
    let neighbors = node.neighbors();
    if neighbors == Neighbors::Start {
        return dir;
    }
    match neighbors.next(&dir) {
        None => panic!("On the loop"),
        Some(n) => fill_loop(map, &npos, n, points),
    }
}

#[derive(PartialEq, Eq)]
enum Crossing {
    FromNorth,
    FromSouth,
    None,
}

// A point is in the loop IFF it crosses the loop path an even number of times
// before reaching the edge. Crossing is a bit tricky though, as
// We may encounter nodes like NE-EW-NW which count as zero crossings
fn count_row(points: &Vec<Status>) -> i32 {
    let mut total = 0;
    let mut in_loop = false;
    let mut entry = Crossing::None;
    for p in points {
        // match p {
        //     Status::NotLoop => print!(" "),
        //     Status::Loop(tile) => print!("{:?}", tile),
        // };
        match p {
            Status::NotLoop => total += in_loop as i32,
            Status::Loop(Tile::NS) => in_loop = !in_loop,
            Status::Loop(Tile::EW | Tile::Empty) => (),
            Status::Loop(Tile::SE) => {
                assert!(entry == Crossing::None);
                entry = Crossing::FromSouth
            }
            Status::Loop(Tile::NE) => {
                assert!(entry == Crossing::None);
                entry = Crossing::FromNorth
            }
            Status::Loop(Tile::SW) => {
                match entry {
                    Crossing::FromNorth => in_loop = !in_loop,
                    Crossing::FromSouth => (),
                    Crossing::None => panic!("We should have crossed here"),
                };
                entry = Crossing::None;
            }
            Status::Loop(Tile::NW) => {
                match entry {
                    Crossing::FromSouth => in_loop = !in_loop,
                    Crossing::FromNorth => (),
                    Crossing::None => panic!("We should have crossed here"),
                };
                entry = Crossing::None;
            }
            Status::Loop(Tile::Start) => panic!("Start should have been substituted"),
        }
    }
    return total;
}

fn main() {
    let input = read_lines();
    let start = find_start(&input);
    let max = P2 {
        x: input[0].len() + 1,
        y: input.len() + 1,
    };
    let (dir, len) = find_loop(&input, &start, &max);
    println!("Part 1 : {}", len / 2 + if len % 2 == 1 { 1 } else { 0 });

    let mut status = input
        .iter()
        .map(|vec| vec.iter().map(|_| Status::NotLoop).collect())
        .collect();
    let dir2 = fill_loop(&input, &start, dir, &mut status);
    use Direction::{E, N, S, W};
    status[start.y - 1][start.x - 1] = Status::Loop(match (dir, dir2.flip()) {
        (N, E) => Tile::NE,
        (N, W) => Tile::NW,
        (N, S) => Tile::NS,
        (S, N) => Tile::NS,
        (S, E) => Tile::SE,
        (S, W) => Tile::SW,
        (E, W) => Tile::EW,
        (E, N) => Tile::NE,
        (E, S) => Tile::SE,
        (W, E) => Tile::EW,
        (W, N) => Tile::NW,
        (W, S) => Tile::SW,
        _ => panic!("Invalid start path"),
    });
    let status = status; // mutability is evil

    let total: i32 = status.iter().map(count_row).sum();
    println!("Part 1 : {}", total);
}
