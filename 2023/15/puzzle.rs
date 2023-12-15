// ==== Puzzle 15 : https://adventofcode.com/2023/day/15 ====

use itertools::Itertools;

fn hash(s: &str) -> usize {
    let mut res = 0;
    for b in s.bytes() {
        res += b as usize;
        res *= 17;
        res &= 0b11111111; // 255
    }
    res
}

fn read_lines() -> Vec<String> {
    std::io::stdin().lines().filter_map(|x| x.ok()).collect()
}

struct Lens<'a> {
    label: &'a str,
    focal: u8,
}

fn parse_sequence<'a>(seq: &'a str, mut map: Vec<Vec<Lens<'a>>>) -> Vec<Vec<Lens<'a>>> {
    let n = seq.find(|c| c == '-' || c == '=').unwrap();
    let focal = if (n + 1) == seq.len() {
        None
    } else {
        Some(seq[n + 1..].parse().unwrap())
    };
    let label = &seq[0..n];
    let hash = hash(&label);
    match (map[hash].iter().find_position(|l| l.label == label), focal) {
        (None, None) => (),
        (Some((i, _)), None) => {
            map[hash].remove(i);
        }
        (None, Some(focal)) => map[hash].push(Lens { label, focal }),
        (Some((i, _)), Some(focal)) => map[hash][i] = Lens { label, focal },
    }
    map
}

fn main() {
    let input = read_lines();
    let p1: usize = input[0].split(",").map(hash).sum();
    println!("Part 1 : {}", p1);

    let mut map = Vec::new();
    for _ in 0..256 {
        map.push(Vec::new());
    }
    for seq in input[0].split(",") {
        map = parse_sequence(seq, map);
    }
    let p2: usize = map
        .iter()
        .enumerate()
        .map(|(b, x)| -> usize {
            (b + 1)
                * x.iter()
                    .enumerate()
                    .map(|(j, l)| (j + 1) * l.focal as usize)
                    .sum::<usize>()
        })
        .sum();
    println!("Part 2 : {}", p2);
}
