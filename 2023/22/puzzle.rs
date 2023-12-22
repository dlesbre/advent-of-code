// ==== Puzzle 22 : https://adventofcode.com/2023/day/22 ====

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Vec3 {
    z: usize, // z first to sort by height
    x: usize,
    y: usize,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Brick {
    start: Vec3,
    end: Vec3,
}

fn parse_vec3(string: &str) -> Vec3 {
    let mut s = string.split(",");
    let x = s.next().unwrap().parse().unwrap();
    let y = s.next().unwrap().parse().unwrap();
    let z = s.next().unwrap().parse().unwrap();
    Vec3 { x, y, z }
}

fn parse(string: String) -> Brick {
    let mut s = string.split("~");
    let start = parse_vec3(s.next().unwrap());
    let end = parse_vec3(s.next().unwrap());
    if end.z < start.z || end.y < start.y || end.x < start.x {
        Brick {
            start: end,
            end: start,
        }
    } else {
        Brick { start, end }
    }
}

fn read_lines() -> Vec<Brick> {
    std::io::stdin()
        .lines()
        .filter_map(|x| x.ok())
        .map(parse)
        .collect()
}

use std::collections::{HashMap, HashSet, VecDeque};

fn main() {
    let mut input = read_lines();
    input.sort();
    let mut top_squares = HashMap::new(); // map(x,y) => (z, brick_id)
    let mut carries: Vec<HashSet<usize>> = Vec::with_capacity(input.len()); // brick_id => vec<brick_id>

    // Construct carries: a map brick_id => set of bricks above
    // using a 2d map top_square: (x, y) => (top of pile, brick_id for top of pile)
    for (id, brick) in input.iter().enumerate() {
        //assert!(brick.start.x <= brick.end.x);
        //assert!(brick.start.y <= brick.end.y);
        //assert!(brick.start.z <= brick.end.z);
        let mut max_z: usize = 1;
        for x in brick.start.x..=brick.end.x {
            for y in brick.start.y..=brick.end.y {
                match top_squares.get(&(x, y)) {
                    None => (),
                    Some((n, _)) => max_z = std::cmp::max(*n, max_z),
                }
            }
        }
        let new_top = brick.end.z - (brick.start.z - max_z);
        for x in brick.start.x..=brick.end.x {
            for y in brick.start.y..=brick.end.y {
                match top_squares.get(&(x, y)) {
                    Some((n, old_id)) if *n == max_z => {
                        let set: &mut HashSet<usize> = &mut carries[*old_id];
                        set.insert(id);
                    }
                    _ => (),
                }
                top_squares.insert((x, y), (new_top + 1, id));
            }
        }
        carries.push(HashSet::new());
    }

    // Construct rests_on, the flipped version of carries
    let mut rests_on = Vec::with_capacity(input.len());
    for _ in &input {
        rests_on.push(HashSet::new());
    }
    for (i, x) in carries.iter().enumerate() {
        for j in x {
            rests_on[*j].insert(i);
        }
    }

    let p1 = carries
        .iter()
        .filter(|x| x.iter().all(|j| rests_on[*j].len() != 1))
        .count();
    println!("Part 1 : {}", p1);

    let mut p2 = 0;
    for j in (0..input.len()).rev() {
        let mut fallen = HashSet::new();
        let mut to_check = VecDeque::new();
        to_check.push_back(j);
        fallen.insert(j);
        // This could probably be cached
        while let Some(j) = to_check.pop_front() {
            for &x in &carries[j] {
                if rests_on[x].is_subset(&fallen) {
                    fallen.insert(x);
                    to_check.push_back(x);
                }
            }
        }
        p2 += fallen.len() - 1;
    }
    println!("Part 2 : {}", p2);
}
