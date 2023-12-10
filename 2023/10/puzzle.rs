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

fn main() {
    let input = read_lines();
    let start = find_start(&input);
    let max = P2 {
        x: input[0].len() + 1,
        y: input.len() + 1,
    };
    let (dir, len) = find_loop(&input, &start, &max);
    println!("Part 1 : {}", len / 2 + if len % 2 == 1 { 1 } else { 0 });
}
