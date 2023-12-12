// ==== Puzzle 12 : https://adventofcode.com/2023/day/12 ====

#[derive(Clone, Copy)]
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
    info: Vec<usize>,
}

struct Precomputed<'a> {
    line: &'a Line,
    max_from_here: Vec<usize>, // max damaged section starting at position,
    remaining_total: Vec<usize>, // remaining total of damage section
}

fn precompute<'a>(line: &'a Line) -> Precomputed<'a> {
    let mut remaining_total = line.info.clone();
    for i in (0..(line.info.len() - 1)).rev() {
        remaining_total[i] += 1 + remaining_total[i + 1];
    }
    let mut max_from_here: Vec<usize> = line.springs.iter().map(|_| 0).collect();
    let mut next = 0;
    for (i, n) in line.springs.iter().enumerate().rev() {
        match n {
            Operational => (),
            Damaged | Unknown => max_from_here[i] += 1 + next,
        }
        next = max_from_here[i];
    }
    Precomputed {
        line,
        remaining_total,
        max_from_here,
    }
}

fn parse_line(line: String) -> Line {
    let mut s = line.split(" ");
    let springs = s.next().unwrap().chars().map(parse_spring).collect();
    let info = s
        .next()
        .unwrap()
        .split(",")
        .filter_map(|c| c.parse().ok())
        .collect();
    Line { springs, info }
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
    let mut info = line.info.clone();
    for _ in 0..4 {
        springs.push(Unknown);
        springs.append(&mut line.springs.clone());
        info.append(&mut line.info.clone());
    }
    Line { springs, info }
}

fn nb_damaged(p: &Precomputed, index_springs: usize, index_info: usize, damaged: usize) -> usize {
    if index_springs == p.line.springs.len() {
        if damaged == 0 && index_info == p.line.info.len() {
            return 1;
        }
        return 0;
    }
    if damaged == 0 {
        return match p.line.springs[index_springs] {
            Damaged => 0,
            Operational | Unknown => nb_undamaged(p, index_springs + 1, index_info),
        };
    }
    match p.line.springs[index_springs] {
        Operational => 0,
        Damaged | Unknown => nb_damaged(p, index_springs + 1, index_info, damaged - 1),
    }
}

fn nb_undamaged(p: &Precomputed, index_springs: usize, index_info: usize) -> usize {
    if index_springs == p.line.springs.len() {
        return if index_info == p.line.info.len() {
            1
        } else {
            0
        };
    }
    if index_info == p.line.info.len() {
        return match p.line.springs[index_springs] {
            Damaged => 0,
            Operational | Unknown => nb_undamaged(p, index_springs + 1, index_info),
        };
    }
    if p.remaining_total[index_info] > p.line.springs.len() - index_springs {
        return 0;
    }
    // Not enough space to fit next segment of damaged before an undamaged spring
    if p.max_from_here[index_springs] < p.line.info[index_info] {
        // skip whole segment
        return nb_undamaged(
            p,
            index_springs + p.max_from_here[index_springs] + 1,
            index_info,
        );
    }
    match p.line.springs[index_springs] {
        Operational => nb_undamaged(p, index_springs + 1, index_info),
        Damaged => nb_damaged(
            p,
            index_springs + 1,
            index_info + 1,
            p.line.info[index_info] - 1,
        ),
        Unknown => {
            let u = nb_undamaged(p, index_springs + 1, index_info);
            let v = nb_damaged(
                p,
                index_springs + 1,
                index_info + 1,
                p.line.info[index_info] - 1,
            );
            u + v
        }
    }
}

fn solve(line: &Line) -> usize {
    nb_undamaged(&precompute(line), 0, 0)
}

fn main() {
    let input = read_lines();
    let p1: usize = input.iter().map(solve).sum();
    println!("Part 1 : {}", p1);

    let p2: usize = input
        .iter()
        .map(|l| {
            println!("Done!");
            solve(&extend(l))
        })
        .sum();
    println!("Part 2 : {}", p2);
}
