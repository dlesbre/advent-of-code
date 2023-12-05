// ==== Puzzle 05 : https://adventofcode.com/2023/day/5 ====

// extern crate regex;

// use regex::Regex;
// use std::{cmp::Ordering, fmt};

fn parse_seeds(line: &str) -> Vec<usize> {
    let mut vec = Vec::new();
    // len("seeds: ") is 7
    for nb in line[7..].split(" ") {
        vec.push(nb.parse().unwrap());
    }
    return vec
}

struct Range {
    len: usize,
    source: usize,
    dest: usize
}

/// parse a line of three numbers into a range
fn parse_range(line: &String) -> Range {
    let mut s = line.split(" ");
    let dest = s.next().unwrap().parse().unwrap();
    let source = s.next().unwrap().parse().unwrap();
    let len = s.next().unwrap().parse().unwrap();
    return Range {
      dest, source, len
    }
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
            i = j+1;
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
// 694734096
fn main() {
    let lines = read_lines();
    let seeds = parse_seeds(&lines[0]);
    let maps: Vec<_> = split_maps(lines[2..].to_vec()).iter().map(parse_map).collect();
    let fina = seeds.iter().map(|s| map_all(*s, &maps)).min().unwrap();
    println!("{}", fina);
}
