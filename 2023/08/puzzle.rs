// ==== Puzzle 08 : https://adventofcode.com/2023/day/8 ====

use std::collections::HashMap;

fn read_lines() -> Vec<String> {
    std::io::stdin().lines().filter_map(|x| x.ok()).collect()
}

enum Direction {
    L,
    R,
}

type Node = [char; 3];

struct NextNode {
    left: Node,
    right: Node,
}

type Map = HashMap<Node, NextNode>;

fn parse_direction(line: &str) -> Vec<Direction> {
    line.chars()
        .map(|c| match c {
            'L' => Direction::L,
            'R' => Direction::R,
            _ => panic!("Unknown direction"),
        })
        .collect()
}

fn str_to_node(str: &str) -> Node {
    str.chars().collect::<Vec<_>>().try_into().unwrap()
}



const START: Node = ['A', 'A', 'A'];
const END: Node = ['Z', 'Z', 'Z'];

fn step<'a>(dir: &Direction, pos: &Node, map: &'a Map) -> &'a Node {
    let node = &map.get(pos).unwrap();
    match dir {
        Direction::L => &node.left,
        Direction::R => &node.right,
    }
}

fn gcd(a: u64, b: u64) -> u64 {
    if b == 0 {
        return a;
    }
    gcd(b, a % b)
}

fn lcm(a: u64, b: u64) -> u64 {
    (a / gcd(a, b)) * b
}

fn time_to_end(part_two: bool, node: &Node, directions: &Vec<Direction>, map: &Map) -> u64 {
    let mut steps = 0;
    let mut n = node;
    for dir in directions.iter().cycle() {
        let end = if part_two { n[2] == 'Z' } else { *n == END };
        if end {
            break;
        }
        n = step(&dir, &n, &map);
        steps += 1;
    }
    return steps;
}

fn main() {
    let lines = read_lines();
    let directions = parse_direction(&lines[0]);
    let map: HashMap<_, _> = lines[2..]
        .iter()
        .map(|s| {
            (
                str_to_node(&s[0..3]),
                NextNode {
                    left: str_to_node(&s[7..10]),
                    right: str_to_node(&s[12..15]),
                },
            )
        })
        .collect();

    // Part 1
    println!(
        "Part 1 : {} steps",
        time_to_end(false, &START, &directions, &map)
    );

    // Part 2
    // This really shouldn't work... If a node has several successful states on
    // its path, and does not end in the first, I suspect this will fail...
    let time = map.keys()
        .filter(|x| x[2] == 'A')
        .map(|n| time_to_end(true, n, &directions, &map))
        .fold(directions.len() as u64, lcm);
    println!("Part 2 : {} steps", time);
}
