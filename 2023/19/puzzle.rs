// ==== Puzzle 19 : https://adventofcode.com/2023/day/19 ====

enum Coord {
    X,
    M,
    A,
    S,
}
use Coord::*;

struct Test {
    coord: Coord,
    is_gt: bool,
    value: usize,
    goto: String,
}

#[derive(Clone)]
struct Part {
    x: usize,
    m: usize,
    a: usize,
    s: usize,
}

#[derive(Clone)]
struct PartRange {
    min: Part,
    max: Part, //excluded
}

impl Part {
    fn rating(&self) -> usize {
        self.x + self.m + self.a + self.s
    }
}

impl PartRange {
    fn nb_combinations(&self) -> usize {
        (self.max.x - self.min.x)
            * (self.max.m - self.min.m)
            * (self.max.a - self.min.a)
            * (self.max.s - self.min.s)
    }
}

impl Test {
    fn check(&self, part: &Part) -> bool {
        let value = match self.coord {
            X => part.x,
            M => part.m,
            A => part.a,
            S => part.s,
        };
        if self.is_gt {
            value > self.value
        } else {
            value < self.value
        }
    }

    fn split_range(&self, range: PartRange) -> (Option<PartRange>, Option<PartRange>) {
        let (min, max) = match self.coord {
            X => (range.min.x, range.max.x),
            M => (range.min.m, range.max.m),
            A => (range.min.a, range.max.a),
            S => (range.min.s, range.max.s),
        };
        if (self.is_gt && min > self.value) || (!self.is_gt && max < self.value) {
            (Some(range), None)
        } else if (self.is_gt && max <= self.value) || (!self.is_gt && min >= self.value) {
            (None, Some(range))
        } else if self.is_gt {
            let mut accepted = range.clone();
            let mut rejected = range;
            match self.coord {
                X => {
                    accepted.min.x = self.value + 1;
                    rejected.max.x = self.value + 1;
                }
                M => {
                    accepted.min.m = self.value + 1;
                    rejected.max.m = self.value + 1;
                }
                A => {
                    accepted.min.a = self.value + 1;
                    rejected.max.a = self.value + 1;
                }
                S => {
                    accepted.min.s = self.value + 1;
                    rejected.max.s = self.value + 1;
                }
            };
            (Some(accepted), Some(rejected))
        } else {
            let mut accepted = range.clone();
            let mut rejected = range.clone();
            match self.coord {
                X => {
                    accepted.max.x = self.value;
                    rejected.min.x = self.value
                }
                M => {
                    accepted.max.m = self.value;
                    rejected.min.m = self.value
                }
                A => {
                    accepted.max.a = self.value;
                    rejected.min.a = self.value
                }
                S => {
                    accepted.max.s = self.value;
                    rejected.min.s = self.value
                }
            };
            (Some(accepted), Some(rejected))
        }
    }

    fn parse(s: &str) -> Test {
        let c = s.as_bytes();
        let coord = match c[0] as char {
            'x' => X,
            'm' => M,
            'a' => A,
            's' => S,
            _ => panic!("Invalid coord"),
        };
        let is_gt = c[1] == '>' as u8;
        let mut iter = s[2..].split(":");
        let value: usize = iter.next().unwrap().parse().unwrap();
        let goto = String::from(iter.next().unwrap());
        Test {
            coord,
            is_gt,
            value,
            goto,
        }
    }
}

fn read_lines() -> Vec<String> {
    std::io::stdin().lines().filter_map(|x| x.ok()).collect()
}

use std::collections::HashMap;

fn parse<'a>(lines: &'a Vec<String>) -> (HashMap<&'a str, (Vec<Test>, &'a str)>, Vec<Part>) {
    let mut found_empty = false;
    let mut map = HashMap::new();
    let mut parts = Vec::new();
    for v in lines {
        if v == "" {
            found_empty = true;
            continue;
        }
        if found_empty {
            let mut iter = v[1..v.len() - 1].split(",");
            let x = iter.next().unwrap()[2..].parse().unwrap();
            let m = iter.next().unwrap()[2..].parse().unwrap();
            let a = iter.next().unwrap()[2..].parse().unwrap();
            let s = iter.next().unwrap()[2..].parse().unwrap();
            parts.push(Part { x, m, a, s });
        } else {
            let mut iter = v[..v.len() - 1].split("{");
            let name = iter.next().unwrap();
            let mut tests = Vec::new();
            let mut next = iter.next().unwrap().split(",").peekable();
            // all but last
            while let Some(word) = next.next() {
                if next.peek().is_none() {
                    map.insert(name, (tests, word));
                    break;
                }
                tests.push(Test::parse(word));
            }
        }
    }
    (map, parts)
}

const STARTING_WORKFLOW: &str = "in";

fn solve_part(workflow: &str, part: &Part, map: &HashMap<&str, (Vec<Test>, &str)>) -> bool {
    match map.get(workflow) {
        None => panic!("No such workflow"),
        Some((tests, fail_case)) => {
            for test in tests {
                if test.check(part) {
                    return match test.goto.as_str() {
                        "A" => true,
                        "R" => false,
                        s => solve_part(s, part, map),
                    };
                }
            }
            match *fail_case {
                "A" => true,
                "R" => false,
                s => solve_part(s, part, map),
            }
        }
    }
}

const MAX: usize = 4001;
const INIT_RANGE: PartRange = PartRange {
    min: Part {
        x: 1,
        m: 1,
        a: 1,
        s: 1,
    },
    max: Part {
        x: MAX,
        m: MAX,
        a: MAX,
        s: MAX,
    },
};

use std::collections::VecDeque;

fn solve_range(map: &HashMap<&str, (Vec<Test>, &str)>) -> usize {
    let mut todo = VecDeque::new();
    todo.push_back((STARTING_WORKFLOW, INIT_RANGE));
    let mut sum = 0;
    'outer: while let Some((workflow, mut range)) = todo.pop_front() {
        match map.get(workflow) {
            None => panic!("No such workflow"),
            Some((tests, fail_case)) => {
                for test in tests {
                    let (accepted, rejected) = test.split_range(range);
                    match accepted {
                        None => (),
                        Some(accepted) => match test.goto.as_str() {
                            "A" => sum += accepted.nb_combinations(),
                            "R" => (),
                            s => todo.push_back((s, accepted)),
                        },
                    };
                    match rejected {
                        None => {
                            continue 'outer;
                        }
                        Some(rejected) => range = rejected,
                    };
                }
                match *fail_case {
                    "A" => sum += range.nb_combinations(),
                    "R" => (),
                    s => todo.push_back((s, range)),
                }
            }
        }
    }
    sum
}

fn main() {
    let lines = read_lines();
    let (map, parts) = parse(&lines);
    let p1: usize = parts
        .iter()
        .filter_map(|p| {
            if solve_part(&STARTING_WORKFLOW, p, &map) {
                Some(p.rating())
            } else {
                None
            }
        })
        .sum();
    println!("Part 1 : {}", p1);

    let p2 = solve_range(&map);
    println!("Part 2 : {}", p2);
}
