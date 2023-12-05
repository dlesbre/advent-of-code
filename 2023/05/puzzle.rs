// ==== Puzzle 05 : https://adventofcode.com/2023/day/5 ====

// extern crate regex;

// use regex::Regex;
// use std::{cmp::Ordering, fmt};

use itertools::Itertools;
use std::cmp::{max, min};

fn parse_seeds(line: &str) -> Vec<usize> {
    let mut vec = Vec::new();
    // len("seeds: ") is 7
    for nb in line[7..].split(" ") {
        vec.push(nb.parse().unwrap());
    }
    return vec;
}

struct Range {
    len: usize,
    source: usize,
    dest: usize,
}

/// parse a line of three numbers into a range
fn parse_range(line: &String) -> Range {
    let mut s = line.split(" ");
    let dest = s.next().unwrap().parse().unwrap();
    let source = s.next().unwrap().parse().unwrap();
    let len = s.next().unwrap().parse().unwrap();
    return Range { dest, source, len };
}

fn parse_map(vec: &Vec<String>) -> Vec<Range> {
    // ignore first line
    return vec[1..].iter().map(parse_range).collect();
}

fn read_lines() -> Vec<String> {
    let mut file_lines: Vec<String> = Vec::new();
    for line in std::io::stdin().lines() {
        match line {
            Ok(l) => file_lines.push(l),
            Err(_) => panic!(),
        }
    }
    return file_lines;
}

fn split_maps(lines: Vec<String>) -> Vec<Vec<String>> {
    let mut maps = Vec::new();
    let mut i = 0;
    for (j, line) in lines.iter().enumerate() {
        if line == "" {
            maps.push(lines[i..j].to_vec());
            i = j + 1;
        }
    }
    maps.push(lines[i..].to_vec());
    return maps;
}

fn map_1(seed: usize, map: &Vec<Range>) -> usize {
    for range in map {
        if range.source <= seed && seed < range.source + range.len {
            // println!("Mapping {} to {}",seed,seed - range.source + range.dest);
            return seed - range.source + range.dest;
        }
    }
    // println!("Mapping {} Unchanges", seed);
    return seed;
}

fn map_all(seed: usize, maps: &Vec<Vec<Range>>) -> usize {
    let mut s = seed;
    for map in maps {
        s = map_1(s, map);
    }
    return s;
}

#[derive(Clone, Copy)]
struct SeedRange {
    start: usize,
    len: usize,
}

fn parse_seeds_2(seeds: &Vec<usize>) -> Vec<SeedRange> {
    let mut vec = Vec::new();
    for (i, (start, len)) in Itertools::tuple_windows(seeds.iter()).enumerate() {
        if i % 2 == 0 {
            vec.push(SeedRange {
                start: *start,
                len: *len,
            });
        }
    }
    return vec;
}

fn map_1_range(seed: SeedRange, map: &Vec<Range>) -> Vec<SeedRange> {
    let mut res = Vec::new();
    for range in map {
        let seed_end = seed.start + seed.len;
        let range_end = range.source + range.len;
        // interval intersection
        if range.source < seed_end && seed.start < range_end {
            // start of seed not in range
            if seed.start < range.source {
                res = map_1_range(
                    SeedRange {
                        start: seed.start,
                        len: range.source - seed.start,
                    },
                    map,
                );
            }
            // end of seed not in range
            if seed_end > range_end {
                let mut to_append = map_1_range(
                    SeedRange {
                        start: range_end,
                        len: seed_end - range_end,
                    },
                    map,
                );
                res.append(&mut to_append);
            }
            // part of seed that is in range
            let start = max(seed.start, range.source) - range.source + range.dest;
            let len = min(seed.len, range.len);
            res.push(SeedRange { start, len });
            return res;
        }
    }
    res.push(seed);
    return res;
}

fn map_all_seeds(seeds: Vec<SeedRange>, map: &Vec<Range>) -> Vec<SeedRange> {
    return seeds.iter().flat_map(|s| map_1_range(*s, map)).collect();
}

fn map_all_2(seeds: Vec<SeedRange>, maps: &Vec<Vec<Range>>) -> Vec<SeedRange> {
    let mut s = seeds;
    for map in maps {
        s = map_all_seeds(s, map);
    }
    return s;
}

fn main() {
    let lines = read_lines();
    let seeds = parse_seeds(&lines[0]);
    let maps: Vec<_> = split_maps(lines[2..].to_vec())
        .iter()
        .map(parse_map)
        .collect();
    let fina = seeds.iter().map(|s| map_all(*s, &maps)).min().unwrap();
    println!("Part 1 : {}", fina);

    let seeds2 = parse_seeds_2(&seeds);
    let res = map_all_2(seeds2, &maps);
    println!("Part 2 : {}", res.iter().map(|c| c.start).min().unwrap());
}
