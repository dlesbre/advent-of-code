// ==== Puzzle 18 : https://adventofcode.com/2023/day/18 ====

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct P2 {
    x: isize,
    y: isize,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, PartialOrd, Ord)]
enum Direction {
    S = 0,
    E = 1,
    W = 3,
    N = 2,
}

fn absolute_diff(x: isize, y: isize) -> isize {
    if x > y {
        x - y
    } else {
        y - x
    }
}

impl P2 {
    fn step(&self, amount: isize, dir: &Direction) -> P2 {
        match dir {
            Direction::N => P2 {
                x: self.x,
                y: self.y - amount,
            },
            Direction::S => P2 {
                x: self.x,
                y: self.y + amount,
            },
            Direction::E => P2 {
                x: self.x + amount,
                y: self.y,
            },
            Direction::W => P2 {
                x: self.x - amount,
                y: self.y,
            },
        }
    }

    fn manhattan(&self, other: &Self) -> isize {
        absolute_diff(self.x, other.x) + absolute_diff(self.y, other.y)
    }
}

struct Instr {
    dir: Direction,
    amount: isize,
    color: usize,
}

fn parse_instr(s: String) -> Instr {
    let mut iter = s.split(" ");
    let dir = match iter.next().unwrap() {
        "U" => Direction::N,
        "D" => Direction::S,
        "L" => Direction::W,
        "R" => Direction::E,
        _ => panic!("Invalid direction"),
    };
    let amount = iter.next().unwrap().parse().unwrap();
    let color = usize::from_str_radix(&iter.next().unwrap()[2..8], 16).unwrap();
    Instr { dir, amount, color }
}
fn read_lines() -> Vec<Instr> {
    std::io::stdin()
        .lines()
        .filter_map(|x| x.ok())
        .map(parse_instr)
        .collect()
}

fn points(input: &Vec<Instr>) -> Vec<P2> {
    let mut p = P2 { x: 0, y: 0 };
    let mut points = Vec::new();
    for step in input {
        let next = p.step(step.amount, &step.dir);
        points.push(p);
        p = next;
    }
    points.push(p);
    points
}

fn points2(input: &Vec<Instr>) -> Vec<P2> {
    let mut p = P2 { x: 0, y: 0 };
    let mut points = Vec::new();
    for step in input {
        let amount = (step.color >> 4) as isize;
        let dir = match step.color & 0xf {
            0 => Direction::E,
            1 => Direction::S,
            2 => Direction::W,
            3 => Direction::N,
            _ => panic!("Unknown direction"),
        };
        let next = p.step(amount, &dir);
        points.push(p);
        p = next;
    }
    points.push(p);
    points
}

fn area(mut points: Vec<P2>) -> isize {
    points.push(points[1].clone());
    let mut area = 0;
    let mut perim = 0; // points[1].manhattan(&points[0]);
    for i in 1..(points.len() - 1) {
        let diff = points[i + 1].y - points[i - 1].y;
        area += points[i].x * diff;
        perim += points[i].manhattan(&points[i - 1]);
    }
    area /= 2;
    // pick's theorem to get number of interior points from area
    let b = area - perim / 2 + 1;
    b + perim // + perim
}

// fn perimeter(points: &Vec<P2>) -> isize {
//     let mut p = 0;
// }

fn main() {
    let input = read_lines();
    let points = points(&input);
    assert!(points[0] == points[points.len() - 1]);
    let p1 = area(points);
    println!("Part 1 : {}", p1);

    let points = points2(&input);
    assert!(points[0] == points[points.len() - 1]);
    let p2 = area(points);
    println!("Part 2 : {}", p2);
}
