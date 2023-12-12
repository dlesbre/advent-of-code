// ==== Puzzle 12 : https://adventofcode.com/2023/day/12 ====

// use std::collections::HashMap;

#[derive(Clone, Copy, PartialEq, Eq)]
enum SpringStatus {
    Damaged,
    Operational,
    Unknown,
}

use SpringStatus::*;

fn parse_spring(c: char) -> SpringStatus {
    match c {
        '#' => Damaged,
        '.' => Operational,
        '?' => Unknown,
        _ => panic!("Unrecognized spring"),
    }
}

struct Line {
    springs: Vec<SpringStatus>,
    chunks: Vec<usize>,
}

/// previous[i] = number of ways to fit chunks 0..n in line[0..i]
/// Sets next[i] = number of ways to fit chunks 0..(n+1) in line[0..i],
/// (taken as argument to avoid reallocating a new array)
/// assumes: chunk is chunk[n]
/// assumes: previous and next have size springs.len() + 1
/// assumes: n > 0
fn dynamic_next(
    springs: &Vec<SpringStatus>,
    previous: &Vec<usize>,
    next: &mut Vec<usize>,
    chunk: usize,
) {
    for i in 0..(chunk + 1) {
        next[i] = 0; // can't fit chunk here
    }
    for i in (chunk + 1)..next.len() {
        match springs[i - 1] {
            Operational => next[i] = next[i - 1],
            Damaged | Unknown => {
                // space before current segment
                let space = i - chunk - 1;
                if springs[space] != Damaged
                    && springs[(i - chunk)..i].iter().all(|c| *c != Operational)
                {
                    next[i] = previous[i - chunk - 1]
                } else {
                    next[i] = 0;
                }
                // case Unkown is Operational
                if springs[i - 1] == Unknown {
                    next[i] += next[i - 1];
                }
            }
        }
    }
}

/// Very similar do dynamic_next, but simpler for first chunk
/// (no need for a leading space, no previous cases)
fn dynamic_init(springs: &Vec<SpringStatus>, chunk: usize) -> Vec<usize> {
    let mut next = Vec::new();
    for _ in 0..chunk {
        next.push(0);
    }
    for i in chunk..(springs.len() + 1) {
        match springs[i - 1] {
            Operational => next.push(next[i - 1]),
            Damaged | Unknown => {
                // block can't be placed after first damaged
                if springs[(i - chunk)..i].iter().all(|c| *c != Operational)
                    && springs[0..(i - chunk)].iter().all(|c| *c != Damaged)
                {
                    next.push(1);
                } else {
                    next.push(0);
                }
                if springs[i - 1] == Unknown {
                    next[i] += next[i - 1];
                }
            }
        }
    }
    next
}

fn dynamic(line: &Line) -> usize {
    let mut prev = dynamic_init(&line.springs, line.chunks[0]);
    let mut next = prev.clone();
    for chunk in line.chunks[1..].iter() {
        dynamic_next(&line.springs, &prev, &mut next, *chunk);
        std::mem::swap(&mut prev, &mut next);
    }
    *prev.last().unwrap()
}

fn parse_line(line: String) -> Line {
    let mut s = line.split(" ");
    let springs = s.next().unwrap().chars().map(parse_spring).collect();
    let chunks = s
        .next()
        .unwrap()
        .split(",")
        .filter_map(|c| c.parse().ok())
        .collect();
    Line { springs, chunks }
}

fn read_lines() -> Vec<Line> {
    std::io::stdin()
        .lines()
        .filter_map(|x| x.ok())
        .map(parse_line)
        .collect()
}

fn extend(line: &Line) -> Line {
    let mut springs = line.springs.clone();
    let mut chunks = line.chunks.clone();
    for _ in 0..4 {
        springs.push(Unknown);
        springs.append(&mut line.springs.clone());
        chunks.append(&mut line.chunks.clone());
    }
    Line { springs, chunks }
}

fn main() {
    let input = read_lines();
    let p1: usize = input.iter().map(dynamic).sum();
    println!("Part 1 : {}", p1);

    let p2: usize = input.iter().map(|l| dynamic(&extend(l))).sum();
    println!("Part 2 : {}", p2);
}
